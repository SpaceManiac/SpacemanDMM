use gfx;
use gfx::traits::{Factory as FactoryTrait, FactoryExt};
use {Resources, Factory, Encoder, ColorFormat, RenderTargetView, Texture};

use ndarray::Axis;

use dm::objtree::ObjectTree;
use dm::constants::Constant;
use dmm_tools::dmm::Map;
use dmm_tools::minimap::{self, Atom};

use dmi::*;

const TILE_SIZE: u32 = 32;

gfx_defines! {
    vertex Vertex {
        position: [f32; 2] = "position",
        color: [f32; 4] = "color",
        uv: [f32; 2] = "uv",
    }

    constant Transform {
        transform: [[f32; 4]; 4] = "transform",
    }

    pipeline pipe {
        vbuf: gfx::VertexBuffer<Vertex> = (),
        transform: gfx::ConstantBuffer<Transform> = "Transform",
        tex: gfx::TextureSampler<[f32; 4]> = "tex",
        out: gfx::BlendTarget<ColorFormat> = ("Target0", gfx::state::ColorMask::all(), gfx::preset::blend::ALPHA),
    }
}

pub struct MapRenderer {
    pub icons: IconCache,
    pub zoom: f32,
    pub layers: [bool; 5],

    pso: gfx::PipelineState<Resources, pipe::Meta>,
    transform_buffer: gfx::handle::Buffer<Resources, Transform>,
    sampler: gfx::handle::Sampler<Resources>,
}

pub struct RenderedMap {
    pub atoms_len: usize,
    pub duration: [f32; 2],

    draw_calls: Vec<DrawCall>,
    ibuf: gfx::IndexBuffer<Resources>,
    vbuf: gfx::handle::Buffer<Resources, Vertex>,
}

struct DrawCall {
    texture: Texture,
    start: u32,
    len: u32,
}

impl MapRenderer {
    pub fn new(factory: &mut Factory, _view: &RenderTargetView) -> MapRenderer {
        let pso = factory.create_pipeline_simple(
            include_bytes!("shaders/main_150.glslv"),
            include_bytes!("shaders/main_150.glslf"),
            pipe::new()
        ).expect("create_pipeline_simple failed");

        let transform_buffer = factory.create_constant_buffer(1);

        let sampler = factory.create_sampler(gfx::texture::SamplerInfo::new(
            gfx::texture::FilterMethod::Scale,
            gfx::texture::WrapMode::Clamp));

        MapRenderer {
            icons: IconCache::default(),
            zoom: 1.0,
            layers: [true; 5],

            pso,
            transform_buffer,
            sampler,
        }
    }

    #[must_use]
    pub fn prepare(&mut self, factory: &mut Factory, objtree: &ObjectTree, map: &Map, z: usize) -> RenderedMap {
        let start = ::std::time::Instant::now();

        // collect the atoms
        let mut atoms = Vec::new();
        for (y, row) in map.z_level(z).axis_iter(Axis(0)).rev().enumerate() {
            for (x, key) in row.iter().enumerate() {
                for fab in map.dictionary[key].iter() {
                    let i = if fab.path.starts_with("/area") {
                        1
                    } else if fab.path.starts_with("/turf") {
                        2
                    } else if fab.path.starts_with("/obj") {
                        3
                    } else if fab.path.starts_with("/mob") {
                        4
                    } else {
                        0
                    };
                    if !self.layers[i] {
                        continue;
                    }
                    let atom = match Atom::from_prefab(objtree, fab, (x as u32, y as u32)) {
                        Some(atom) => atom,
                        None => continue,
                    };
                    match atom.get_var("icon", objtree) {
                        &Constant::Resource(ref path) | &Constant::String(ref path) => {
                            let _ = self.icons.retrieve(factory, path.as_ref());
                        },
                        _ => continue,
                    }
                    atoms.push(atom);
                }
            }
        }

        let midpoint = ::std::time::Instant::now();

        // z sort - TODO: use depth buffer instead?
        atoms.sort_by_key(|a| minimap::layer_of(objtree, a));

        // render atoms
        let mut vertices = Vec::new();
        let mut indices: Vec<u32> = Vec::new();
        let mut draw_calls: Vec<DrawCall> = Vec::new();

        for atom in atoms.iter() {
            let icon = match atom.get_var("icon", objtree) {
                &Constant::Resource(ref path) | &Constant::String(ref path) => path,
                _ => continue,
            };
            let icon_state = match atom.get_var("icon_state", objtree) {
                &Constant::String(ref string) => string,
                _ => "",
            };
            let dir = atom.get_var("dir", objtree).to_int().unwrap_or(::dmi::SOUTH);

            let icon_file = match self.icons.retrieve(factory, icon.as_ref()) {
                Some(icon_file) => icon_file,
                None => continue,
            };
            let width = icon_file.metadata.width as f32;
            let height = icon_file.metadata.height as f32;

            let uv = match icon_file.uv_of(&icon_state, dir) {
                Some(rect) => rect,
                None => continue,
            };

            let pixel_x = atom.get_var("pixel_x", objtree).to_int().unwrap_or(0);
            let pixel_y = atom.get_var("pixel_y", objtree).to_int().unwrap_or(0);
                // + icon_file.metadata.height as i32

            let mut loc = (
                (((atom.loc.0) * TILE_SIZE) as i32 + pixel_x) as f32,
                (((atom.loc.1) * TILE_SIZE) as i32 + pixel_y) as f32,
            );

            let color = minimap::color_of(objtree, atom);
            let color = [
                color[0] as f32 / 255.0, color[1] as f32 / 255.0,
                color[2] as f32 / 255.0, color[3] as f32 / 255.0];

            let start = vertices.len() as u32;
            vertices.extend_from_slice(&[
                Vertex { color, position: [loc.0, loc.1], uv: [uv.0, uv.3] },
                Vertex { color, position: [loc.0, loc.1 + height], uv: [uv.0, uv.1] },
                Vertex { color, position: [loc.0 + width, loc.1 + height], uv: [uv.2, uv.1] },
                Vertex { color, position: [loc.0 + width, loc.1], uv: [uv.2, uv.3] },
            ]);
            let i_start = indices.len() as u32;
            indices.extend_from_slice(&[start, start+1, start+3, start+1, start+2, start+3]);

            if let Some(call) = draw_calls.last_mut() {
                if call.texture == icon_file.texture {
                    call.len += 6;
                    continue;
                }
            }
            draw_calls.push(DrawCall {
                texture: icon_file.texture.clone(),
                start: i_start,
                len: 6,
            });
        }

        let vbuf = factory.create_vertex_buffer(&vertices[..]);
        let ibuf = factory.create_index_buffer(&indices[..]);

        let end = ::std::time::Instant::now();
        RenderedMap {
            atoms_len: atoms.len(),
            duration: [to_seconds(midpoint - start), to_seconds(end - midpoint)],
            draw_calls,

            ibuf,
            vbuf,
        }
    }
}

impl RenderedMap {
    pub fn draw_calls(&self) -> usize {
        self.draw_calls.len()
    }

    pub fn paint(&mut self, parent: &mut MapRenderer, center: [f32; 2], encoder: &mut Encoder, view: &RenderTargetView) {
        let (x, y, _, _) = view.get_dimensions();

        // (0, 0) is the center of the screen, 1.0 = 1 pixel
        let transform = Transform {
            transform: [
                [2.0 / x as f32, 0.0, 0.0, -2.0 * center[0].round() / x as f32],
                [0.0, 2.0 / y as f32, 0.0, -2.0 * center[1].round() / y as f32],
                [0.0, 0.0, 1.0, 0.0],
                [0.0, 0.0, 0.0, 1.0 / parent.zoom],
            ]
        };
        encoder.update_buffer(&parent.transform_buffer, &[transform], 0).expect("update_buffer failed");

        for call in self.draw_calls.iter() {
            let slice = gfx::Slice {
                start: call.start,
                end: call.start + call.len,
                base_vertex: 0,
                instances: None,
                buffer: self.ibuf.clone(),
            };
            let data = pipe::Data {
                vbuf: self.vbuf.clone(),
                transform: parent.transform_buffer.clone(),
                tex: (call.texture.clone(), parent.sampler.clone()),
                out: view.clone(),
            };
            encoder.draw(&slice, &parent.pso, &data);
        }
    }
}

fn to_seconds(duration: ::std::time::Duration) -> f32 {
    duration.as_secs() as f32 + duration.subsec_nanos() as f32 / 1_000_000_000.0
}
