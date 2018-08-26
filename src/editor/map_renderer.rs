use gfx;
use gfx::traits::{Factory as FactoryTrait, FactoryExt};
use {Resources, Factory, Encoder, ColorFormat, RenderTargetView};

use ndarray::Axis;

use dm::objtree::ObjectTree;
use dm::constants::Constant;
use dmm_tools::dmm::{Map, Grid};
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
    pub center: [f32; 2],

    slice: gfx::Slice<Resources>,
    pso: gfx::PipelineState<Resources, pipe::Meta>,
    data: pipe::Data<Resources>,
}

impl MapRenderer {
    pub fn new(factory: &mut Factory, view: &RenderTargetView) -> MapRenderer {
        let mut icons = IconCache::default();
        let (slice, pso, data);
        {
            pso = factory.create_pipeline_simple(
                include_bytes!("shaders/main_150.glslv"),
                include_bytes!("shaders/main_150.glslf"),
                pipe::new()
            ).expect("create_pipeline_simple failed");

            let sampler = factory.create_sampler(gfx::texture::SamplerInfo::new(
                gfx::texture::FilterMethod::Scale,
                gfx::texture::WrapMode::Clamp));

            let test_icon = icons.retrieve(factory, "icons/obj/device.dmi".as_ref()).expect("test icon");
            let w = test_icon.width as f32 / 2.0;
            let h = test_icon.height as f32 / 2.0;
            let (vertex_buffer, new_slice) = factory.create_vertex_buffer_with_slice(&[
                Vertex { position: [-w,  h], color: [1.0, 1.0, 1.0, 1.0], uv: [0.0, 0.0] },
                Vertex { position: [-w, -h], color: [1.0, 1.0, 1.0, 1.0], uv: [0.0, 1.0] },
                Vertex { position: [ w, -h], color: [1.0, 1.0, 1.0, 1.0], uv: [1.0, 1.0] },
                Vertex { position: [ w,  h], color: [1.0, 1.0, 1.0, 1.0], uv: [1.0, 0.0] },
            ], &[0u16, 1, 3, 1, 2, 3][..]);
            slice = new_slice;
            let transform_buffer = factory.create_constant_buffer(1);

            data = pipe::Data {
                vbuf: vertex_buffer,
                transform: transform_buffer,
                tex: (test_icon.texture.clone(), sampler),
                out: view.clone(),
            };
        }

        MapRenderer {
            icons,
            zoom: 1.0,
            center: [0.0; 2],

            slice,
            pso,
            data,
        }
    }

    pub fn prepare(&mut self, factory: &mut Factory, objtree: &ObjectTree, map: &Map, grid: Grid) {
        let (_len_y, _len_x) = grid.dim();

        // collect the atoms
        let mut atoms = Vec::new();
        for (y, row) in grid.axis_iter(Axis(0)).rev().enumerate() {
            for (x, key) in row.iter().enumerate() {
                for fab in map.dictionary[key].last() {  // TODO: change last() to iter()
                    atoms.extend(Atom::from_prefab(objtree, fab, (x as u32, y as u32)));
                }
            }
        }

        // z sort - TODO: use depth buffer instead?
        atoms.sort_by_key(|a| minimap::layer_of(objtree, a));

        // render atoms
        let mut vertices = Vec::new();
        let mut indices: Vec<u32> = Vec::new();

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
            self.data.tex.0 = icon_file.texture.clone();

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
                Vertex { color, position: [loc.0, loc.1 + TILE_SIZE as f32], uv: [uv.0, uv.1] },
                Vertex { color, position: [loc.0 + TILE_SIZE as f32, loc.1 + TILE_SIZE as f32], uv: [uv.2, uv.1] },
                Vertex { color, position: [loc.0 + TILE_SIZE as f32, loc.1], uv: [uv.2, uv.3] },
            ]);
            indices.extend_from_slice(&[start, start+1, start+3, start+1, start+2, start+3]);
        }

        let (vertex_buffer, new_slice) = factory.create_vertex_buffer_with_slice(&vertices, &indices[..]);
        self.data.vbuf = vertex_buffer;
        self.slice = new_slice;
    }

    pub fn paint(&mut self, _factory: &mut Factory, encoder: &mut Encoder, view: &RenderTargetView) {
        self.data.out = view.clone();

        let (x, y, _, _) = view.get_dimensions();

        // (0, 0) is the center of the screen, 1.0 = 1 pixel
        let transform = Transform {
            transform: [
                [2.0 / x as f32, 0.0, 0.0, self.center[0].round() / x as f32],
                [0.0, 2.0 / y as f32, 0.0, -self.center[1].round() / y as f32],
                [0.0, 0.0, 1.0, 0.0],
                [0.0, 0.0, 0.0, 1.0 / self.zoom],
            ]
        };
        encoder.update_buffer(&self.data.transform, &[transform], 0).expect("update_buffer failed");
        encoder.draw(&self.slice, &self.pso, &self.data);
    }
}
