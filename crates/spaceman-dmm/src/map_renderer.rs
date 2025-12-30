//! GPU map renderer.
use crate::dmi::*;
use crate::map_repr::AtomMap;
use crate::ColorFormat;
use dmm_tools::dmm::Prefab;
use dmm_tools::minimap::{Layer, Sprite};
use dreammaker::objtree::ObjectTree;
use imgui::TextureId;
use sdl3::gpu::{
    BlendFactor, Buffer, BufferUsageFlags, ColorTargetBlendState, ColorTargetDescription,
    CommandBuffer, Device, GraphicsPipeline, Sampler, SamplerCreateInfo, Texture,
};
use sdl3::video::Window;
use slice_of_array::prelude::*;
use std::sync::Arc;
use std::time::Instant;

const TILE_SIZE: u32 = 32;

#[derive(Default, Debug, Clone, Copy)]
pub struct Vertex {
    pub position: [f32; 2],
    pub color: [f32; 4],
    pub uv: [f32; 2],
}

pub struct Transform {
    pub transform: [[f32; 4]; 4],
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct RenderPop {
    pub texture: u32,   // icon
    pub size: [f32; 2], // icon

    pub uv: [f32; 4],    // icon_state + dir
    pub color: [f32; 4], // color + alpha
    // TODO: transform
    pub ofs_x: i32, // pixel_x + pixel_w + step_x
    pub ofs_y: i32, // pixel_y + pixel_z + step_y

    pub plane: i32,
    pub layer: Layer,
}

// pipeline pipe {
//     vbuf: gfx::VertexBuffer<Vertex> = (),
//     transform: gfx::ConstantBuffer<Transform> = "Transform",
//     tex: gfx::TextureSampler<[f32; 4]> = "tex",
//     out: gfx::BlendTarget<ColorFormat> = ("Target0", gfx::state::ColorMask::all(), gfx::preset::blend::ALPHA),
// }

pub struct MapRenderer {
    pub icons: Arc<IconCache>,
    pub icon_textures: TextureCache,
    pub zoom: f32,
    pub layers: [bool; 5],

    pub device: Device,
    // pipeline: GraphicsPipeline,
    // transform_buffer: Buffer,
    pub sampler: Sampler,
}

pub struct RenderedMap {
    pub duration: [f32; 2],
    pub thumbnail: Texture<'static>,
    // pub thumbnail_target: RenderTargetView,
    pub thumbnail_id: Option<TextureId>,

    vbuf: Buffer,
    ibuf: Buffer,
}

#[derive(Debug, Clone)]
pub struct DrawCall {
    pub texture: u32,
    pub len: u32,
}

impl MapRenderer {
    pub fn new(device: &Device) -> MapRenderer {
        MapRenderer {
            icons: Arc::new(IconCache::new(".".as_ref())),
            icon_textures: Default::default(),
            zoom: 1.0,
            layers: [true, false, true, true, true],
            device: device.clone(),
            sampler: device
                .create_sampler(SamplerCreateInfo::new())
                .expect("create_sampler"),
            // sampler: (),
        }
    }
}

/*
impl MapRenderer {
    pub fn new(device: &Device, window: &Window) -> MapRenderer {
        let target = ColorTargetDescription::new()
            .with_format(device.get_swapchain_texture_format(window))
            .with_blend_state(
                ColorTargetBlendState::new()
                    .with_src_color_blendfactor(BlendFactor::SrcAlpha)
                    .with_dst_color_blendfactor(BlendFactor::OneMinusSrcAlpha)
                    .with_color_blend_op(sdl3::gpu::BlendOp::Add)
                    .with_src_alpha_blendfactor(BlendFactor::One)
                    .with_dst_alpha_blendfactor(BlendFactor::DstAlpha)
                    .with_alpha_blend_op(sdl3::gpu::BlendOp::Add)
                    .with_enable_blend(true),
            );

        let pso = device
            .create_pipeline_simple(
                include_bytes!("shaders/main_150.glslv"),
                include_bytes!("shaders/main_150.glslf"),
                pipe::new(),
            )
            .expect("create_pipeline_simple failed");

        let transform_buffer = device
            .create_buffer()
            .with_usage(BufferUsageFlags::GRAPHICS_STORAGE_READ)
            .with_size(std::mem::size_of::<Transform>() as u32);

        let sampler = device.create_sampler(gfx::texture::SamplerInfo::new(
            gfx::texture::FilterMethod::Scale,
            gfx::texture::WrapMode::Clamp,
        ));

        MapRenderer {
            icons: Arc::new(IconCache::new(".".as_ref())),
            icon_textures: TextureCache::default(),
            zoom: 1.0,
            layers: [true, false, true, true, true],

            device: device.clone(),
            pso,
            transform_buffer,
            sampler,
        }
    }

    #[must_use]
    pub fn render(&mut self, map: &AtomMap, z: u32) -> RenderedMap {
        let start = Instant::now();

        let vbuf_data = map.vertex_buffer(z).flat();
        let ibuf_data = map.index_buffer(z);
        let ibuf_data = ibuf_data.flat();

        let vbuf = self
            .device
            .create_buffer::<Vertex>(
                vbuf_data.len(),
                gfx::buffer::Role::Vertex,
                gfx::memory::Usage::Dynamic,
                gfx::memory::Bind::empty(),
            )
            .expect("create vertex buffer");
        let ibuf = self
            .device
            .create_buffer::<u32>(
                ibuf_data.len(),
                gfx::buffer::Role::Index,
                gfx::memory::Usage::Dynamic,
                gfx::memory::Bind::empty(),
            )
            .expect("create index buffer");

        let texture = self
            .device
            .create_texture::<gfx::format::R8_G8_B8_A8>(
                gfx::texture::Kind::D2(
                    crate::THUMBNAIL_SIZE,
                    crate::THUMBNAIL_SIZE,
                    gfx::texture::AaMode::Single,
                ),
                1,
                gfx::memory::Bind::RENDER_TARGET | gfx::memory::Bind::SHADER_RESOURCE,
                gfx::memory::Usage::Data,
                Some(gfx::format::ChannelType::Unorm),
            )
            .expect("create thumbnail texture");
        let thumbnail = self
            .device
            .view_texture_as_shader_resource::<ColorFormat>(
                &texture,
                (0, 0),
                gfx::format::Swizzle::new(),
            )
            .expect("view thumbnail as shader resource");
        let thumbnail_target = self
            .device
            .view_texture_as_render_target::<ColorFormat>(&texture, 0, None)
            .expect("view thumbnail as render target");

        RenderedMap {
            duration: [to_seconds(map.duration), to_seconds(Instant::now() - start)],
            thumbnail,
            thumbnail_target,
            thumbnail_id: None,

            vbuf,
            ibuf,
        }
    }
}

impl RenderedMap {
    fn update_buffers(
        &mut self,
        map: &AtomMap,
        z: u32,
        device: &Device,
        command_buffer: &CommandBuffer,
    ) {
        let vbuf_data = map.vertex_buffer(z).flat();
        let ibuf_data = map.index_buffer(z);
        let ibuf_data = ibuf_data.flat();

        if self.vbuf.len() < vbuf_data.len() {
            self.vbuf = device
                .create_buffer::<Vertex>(
                    vbuf_data.len(),
                    gfx::buffer::Role::Vertex,
                    gfx::memory::Usage::Dynamic,
                    gfx::memory::Bind::empty(),
                )
                .expect("create vertex buffer");
        }
        command_buffer
            .update_buffer(&self.vbuf, vbuf_data, 0)
            .expect("update vbuf");

        if self.ibuf.len() < ibuf_data.len() {
            self.ibuf = device
                .create_buffer::<u32>(
                    ibuf_data.len(),
                    gfx::buffer::Role::Index,
                    gfx::memory::Usage::Dynamic,
                    gfx::memory::Bind::empty(),
                )
                .expect("create index buffer");
        }
        command_buffer
            .update_buffer(&self.ibuf, ibuf_data, 0)
            .expect("update ibuf");
    }

    fn inner_paint(
        &self,
        parent: &mut MapRenderer,
        map: &AtomMap,
        z: u32,
        device: &Device,
        command_buffer: &CommandBuffer,
        view: &RenderTargetView,
        transform: [[f32; 4]; 4],
    ) {
        command_buffer
            .update_buffer(&parent.transform_buffer, &[Transform { transform }], 0)
            .expect("update_buffer failed");

        let mut start = 0;
        for call in map.levels[z as usize].draw_calls.iter() {
            let texture =
                parent
                    .icon_textures
                    .retrieve(device, &parent.icons, call.texture as usize);
            let slice = gfx::Slice {
                start: start,
                end: start + call.len,
                base_vertex: 0,
                instances: None,
                buffer: gfx::IndexBuffer::Index32(self.ibuf.clone()),
            };
            // TODO: use borrowing to avoid having to clone so much here?
            let data = pipe::Data {
                vbuf: self.vbuf.clone(),
                transform: parent.transform_buffer.clone(),
                tex: (texture.clone(), parent.sampler.clone()),
                out: view.clone(),
            };
            command_buffer.draw(&slice, &parent.pso, &data);
            start += call.len;
        }
    }

    pub fn paint(
        &mut self,
        parent: &mut MapRenderer,
        map: &AtomMap,
        z: u32,
        center: [f32; 2],
        device: &Device,
        command_buffer: &CommandBuffer,
        view: &RenderTargetView,
    ) {
        // update vertex and index buffers from the map
        if map.levels[z as usize].buffers_dirty.replace(false) {
            self.update_buffers(map, z, device, command_buffer);
        }

        // thumbnail render
        command_buffer.clear(&self.thumbnail_target, [0.0, 0.0, 0.0, 0.0]);
        self.inner_paint(
            parent,
            map,
            z,
            device,
            command_buffer,
            &self.thumbnail_target,
            [
                [2.0 / map.size.0 as f32 / TILE_SIZE as f32, 0.0, 0.0, -1.0],
                [0.0, -2.0 / map.size.1 as f32 / TILE_SIZE as f32, 0.0, 1.0],
                [0.0, 0.0, 1.0, 0.0],
                [0.0, 0.0, 0.0, 1.0],
            ],
        );

        // regular render
        let (x, y, _, _) = view.get_dimensions();
        let zoom = parent.zoom;
        self.inner_paint(
            parent,
            map,
            z,
            device,
            command_buffer,
            view,
            [
                // (0, 0) is the center of the screen, 1.0 = 1 pixel
                [
                    2.0 / x as f32,
                    0.0,
                    0.0,
                    -2.0 * (center[0] * zoom).round() / zoom / x as f32,
                ],
                [
                    0.0,
                    2.0 / y as f32,
                    0.0,
                    -2.0 * (center[1] * zoom).round() / zoom / y as f32,
                ],
                [0.0, 0.0, 1.0, 0.0],
                [0.0, 0.0, 0.0, 1.0 / zoom],
            ],
        );
    }
}
*/

// forgive me
impl std::cmp::Eq for RenderPop {}

impl std::hash::Hash for RenderPop {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
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
        RenderPop::from_sprite(icons, &Sprite::from_vars(objtree, &fab))
    }

    pub fn from_sprite(icons: &IconCache, sprite: &Sprite) -> Option<RenderPop> {
        if sprite.icon.is_empty() {
            return None;
        }

        let texture_id = match icons.get_index(sprite.icon.as_ref()) {
            Some(id) => id,
            None => return None, // couldn't load
        };
        let icon_file = icons.get_icon(texture_id);
        let width = icon_file.metadata.width as f32;
        let height = icon_file.metadata.height as f32;
        let uv = match icon_file.uv_of(sprite.icon_state, sprite.dir) {
            Some(rect) => rect,
            None => return None,
        };

        let color = [
            sprite.color[0] as f32 / 255.0,
            sprite.color[1] as f32 / 255.0,
            sprite.color[2] as f32 / 255.0,
            sprite.color[3] as f32 / 255.0,
        ];

        Some(RenderPop {
            texture: texture_id as u32,
            uv,
            color,
            size: [width, height],
            ofs_x: sprite.ofs_x,
            ofs_y: sprite.ofs_y,
            plane: sprite.plane,
            layer: sprite.layer,
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
            Vertex {
                color,
                position: [loc.0, loc.1],
                uv: [uv[0], uv[3]],
            },
            Vertex {
                color,
                position: [loc.0, loc.1 + height],
                uv: [uv[0], uv[1]],
            },
            Vertex {
                color,
                position: [loc.0 + width, loc.1 + height],
                uv: [uv[2], uv[1]],
            },
            Vertex {
                color,
                position: [loc.0 + width, loc.1],
                uv: [uv[2], uv[3]],
            },
        ]
    }

    pub fn sort_key(&self) -> impl Ord {
        (self.plane, self.layer, self.texture)
    }
}

impl DrawCall {
    pub fn can_contain(&self, rpop: &RenderPop) -> bool {
        self.texture == rpop.texture
    }
}

fn to_seconds(duration: std::time::Duration) -> f32 {
    duration.as_secs() as f32 + duration.subsec_nanos() as f32 / 1_000_000_000.0
}
