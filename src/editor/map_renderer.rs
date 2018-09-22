//! GPU map renderer.
use std::time::Instant;

use gfx;
use gfx::traits::{Factory as FactoryTrait, FactoryExt};
use {Resources, Factory, Encoder, ColorFormat, RenderTargetView, Texture};

use ndarray::Axis;

use dm::objtree::ObjectTree;
use dm::constants::Constant;
use dmm_tools::dmm::{Map, Prefab};
use dmm_tools::minimap::{self, GetVar};

use dmi::*;

const TILE_SIZE: u32 = 32;

gfx_defines! {
    #[derive(Default)]
    vertex Vertex {
        position: [f32; 2] = "position",
        color: [f32; 4] = "color",
        uv: [f32; 2] = "uv",
    }

    constant Transform {
        transform: [[f32; 4]; 4] = "transform",
    }

    #[derive(PartialOrd, Default)]
    constant RenderPop {
        category: u32 = "category",
        texture: u32 = "texture",  // icon
        size: [f32; 2] = "size",  // icon

        uv: [f32; 4] = "uv",  // icon_state + dir
        color: [f32; 4] = "color",  // color + alpha
        // TODO: transform
        ofs_x: i32 = "ofs_x",  // pixel_x + pixel_w + step_x
        ofs_y: i32 = "ofs_y",  // pixel_y + pixel_z + step_y

        plane: i32 = "plane",
        layer: i32 = "layer",
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
    pub icon_textures: TextureCache,
    pub zoom: f32,
    pub layers: [bool; 5],

    pso: gfx::PipelineState<Resources, pipe::Meta>,
    transform_buffer: gfx::handle::Buffer<Resources, Transform>,
    pub sampler: gfx::handle::Sampler<Resources>,
}

pub struct PreparedMap {
    duration: f32,
    pops: Vec<RenderPop>,
    vertices: Vec<Vertex>,
    instances: Vec<(u32, usize)>,
}

pub struct RenderedMap {
    pub atoms_len: usize,
    pub pops_len: usize,
    pub duration: [f32; 2],

    draw_calls: Vec<DrawCall>,
    ibuf: gfx::IndexBuffer<Resources>,
    vbuf: gfx::handle::Buffer<Resources, Vertex>,
}

struct DrawCall {
    category: u32,
    texture_id: u32,
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
            icons: IconCache::new(".".as_ref()),
            icon_textures: TextureCache::default(),
            zoom: 1.0,
            layers: [true, false, true, true, true],

            pso,
            transform_buffer,
            sampler,
        }
    }

    #[must_use]
    pub fn prepare(&mut self, objtree: &ObjectTree, map: &Map, z: usize) -> PreparedMap {
        use std::collections::BTreeMap;

        let start = Instant::now();

        // create pops from the dictionary
        let mut pops = Vec::new();
        let mut pop_reverse = BTreeMap::new();
        let mut pop_dictionary = BTreeMap::new();
        for (key, prefabs) in map.dictionary.iter() {
            let mut all = Vec::new();
            for fab in prefabs {
                if let Some(pop) = RenderPop::from_prefab(&mut self.icons, objtree, fab) {
                    let idx;
                    if let Some(&i) = pop_reverse.get(&pop) {
                        idx = i;
                    } else {
                        idx = pops.len();
                        pop_reverse.insert(pop.clone(), idx);
                        pops.push(pop);
                    }
                    all.push(idx);
                }
            }
            pop_dictionary.insert(*key, all);
        }

        // create instances from the grid
        let mut vertices = Vec::new();
        let mut instances = Vec::new();
        for (y, row) in map.z_level(z).axis_iter(Axis(0)).rev().enumerate() {
            for (x, key) in row.iter().enumerate() {
                for &pop_id in pop_dictionary[key].iter() {
                    let v_start = vertices.len() as u32;
                    vertices.extend_from_slice(&pops[pop_id].instance((x as u32, y as u32)));
                    instances.push((v_start, pop_id));
                }
            }
        }

        // sort instances
        instances.sort_by_key(|&(_, pop_id)| pops[pop_id].sort_key());

        let duration = to_seconds(Instant::now() - start);

        PreparedMap {
            duration,
            pops,
            vertices,
            instances,
        }
    }
}

impl PreparedMap {
    #[must_use]
    pub fn render(&self, parent: &mut MapRenderer, factory: &mut Factory) -> RenderedMap {
        let start = Instant::now();

        // TODO: determine how much of this loop belongs in prepare()
        // create index buffer
        let mut indices = Vec::new();
        let mut draw_calls: Vec<DrawCall> = Vec::new();
        for &(start, pop_id) in self.instances.iter() {
            let i_start = indices.len() as u32;
            indices.extend_from_slice(&[start, start+1, start+3, start+1, start+2, start+3]);

            let pop = self.pops[pop_id];
            if let Some(call) = draw_calls.last_mut() {
                if call.texture_id == pop.texture && call.category == pop.category {
                    call.len += 6;
                    continue;
                }
            }
            let texture = parent.icon_textures.retrieve(factory, &parent.icons, pop.texture as usize);
            draw_calls.push(DrawCall {
                category: pop.category,
                texture_id: pop.texture,
                texture: texture.clone(),
                start: i_start,
                len: 6,
            });
        }

        let vbuf = factory.create_vertex_buffer(&self.vertices[..]);
        let ibuf = factory.create_index_buffer(&indices[..]);

        RenderedMap {
            atoms_len: self.instances.len(),
            pops_len: self.pops.len(),
            duration: [self.duration, to_seconds(Instant::now() - start)],
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
            if !parent.layers[call.category as usize] {
                continue;
            }
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

// forgive me
impl ::std::cmp::Eq for RenderPop {}

impl ::std::cmp::Ord for RenderPop {
    fn cmp(&self, other: &Self) -> ::std::cmp::Ordering {
        self.partial_cmp(other).expect("in RenderPop::cmp, a field was NaN")
    }
}

impl ::std::hash::Hash for RenderPop {
    fn hash<H: ::std::hash::Hasher>(&self, state: &mut H) {
        self.category.hash(state);
        self.texture.hash(state);
        for each in self.size.iter() {
            state.write_u32(each.to_bits());
        }
        for each in self.uv.iter() {
            state.write_u32(each.to_bits());
        }
        for each in self.color.iter() {
            state.write_u32(each.to_bits());
        }
        self.ofs_x.hash(state);
        self.ofs_y.hash(state);
        self.plane.hash(state);
        self.layer.hash(state);
    }
}

impl RenderPop {
    pub fn from_prefab(icons: &IconCache, objtree: &ObjectTree, fab: &Prefab) -> Option<RenderPop> {
        let icon = match fab.get_var("icon", objtree) {
            &Constant::Resource(ref path) | &Constant::String(ref path) => path,
            _ => return None,
        };
        let icon_state = match fab.get_var("icon_state", objtree) {
            &Constant::String(ref string) => string,
            _ => "",
        };
        let dir = fab.get_var("dir", objtree).to_int().unwrap_or(::dmi::SOUTH);

        let (width, height, uv);
        let texture_id = match icons.get_index(icon.as_ref()) {
            Some(id) => id,
            None => return None,  // couldn't load
        };
        {
            let icon_file = icons.get_icon(texture_id);
            width = icon_file.metadata.width as f32;
            height = icon_file.metadata.height as f32;
            uv = match icon_file.uv_of(&icon_state, dir) {
                Some(rect) => rect,
                None => return None,
            };
        }

        let color = minimap::color_of(objtree, fab);
        let color = [
            color[0] as f32 / 255.0, color[1] as f32 / 255.0,
            color[2] as f32 / 255.0, color[3] as f32 / 255.0];

        let pixel_x = fab.get_var("pixel_x", objtree).to_int().unwrap_or(0);
        let pixel_y = fab.get_var("pixel_y", objtree).to_int().unwrap_or(0);
        let pixel_w = fab.get_var("pixel_w", objtree).to_int().unwrap_or(0);
        let pixel_z = fab.get_var("pixel_z", objtree).to_int().unwrap_or(0);
        let step_x = fab.get_var("step_x", objtree).to_int().unwrap_or(0);
        let step_y = fab.get_var("step_y", objtree).to_int().unwrap_or(0);

        Some(RenderPop {
            category: category_of(&fab.path) as u32,
            texture: texture_id as u32,
            uv,
            color,
            size: [width, height],
            ofs_x: pixel_x + pixel_w + step_x,
            ofs_y: pixel_y + pixel_z + step_y,
            plane: minimap::plane_of(objtree, fab),
            layer: minimap::layer_of(objtree, fab),
        })
    }

    pub fn instance(&self, loc: (u32, u32)) -> [Vertex; 4] {
        let uv = self.uv;
        let loc = (
            ((loc.0 * TILE_SIZE) as i32 + self.ofs_x) as f32,
            ((loc.1 * TILE_SIZE) as i32 + self.ofs_y) as f32,
        );
        let (width, height) = (self.size[0], self.size[1]);
        let color = self.color;

        [
            Vertex { color, position: [loc.0, loc.1], uv: [uv[0], uv[3]] },
            Vertex { color, position: [loc.0, loc.1 + height], uv: [uv[0], uv[1]] },
            Vertex { color, position: [loc.0 + width, loc.1 + height], uv: [uv[2], uv[1]] },
            Vertex { color, position: [loc.0 + width, loc.1], uv: [uv[2], uv[3]] },
        ]
    }

    pub fn sort_key(&self) -> impl Ord {
        (self.plane, self.layer, self.texture)
    }
}

fn to_seconds(duration: ::std::time::Duration) -> f32 {
    duration.as_secs() as f32 + duration.subsec_nanos() as f32 / 1_000_000_000.0
}

fn category_of(path: &str) -> u32 {
    if path.starts_with("/area") {
        1
    } else if path.starts_with("/turf") {
        2
    } else if path.starts_with("/obj") {
        3
    } else if path.starts_with("/mob") {
        4
    } else {
        0
    }
}
