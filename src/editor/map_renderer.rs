use gfx;
use gfx::traits::{Factory as FactoryTrait, FactoryExt};
use {Resources, Factory, Encoder, ColorFormat, RenderTargetView};

use dm::objtree::ObjectTree;
use dmm_tools::dmm::{Map, Grid};

use dmi::*;

gfx_defines! {
    vertex Vertex {
        position: [f32; 2] = "position",
        color: [f32; 3] = "color",
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
            let (vertex_buffer, new_slice) = factory.create_vertex_buffer_with_slice(&[
                Vertex { position: [-(test_icon.width as f32) / 2.0, -(test_icon.height as f32) / 2.0], color: [1.0, 1.0, 1.0], uv: [0.0, 1.0] },
                Vertex { position: [ 0.0,  test_icon.height as f32 / 2.0], color: [1.0, 1.0, 1.0], uv: [0.5, 0.0] },
                Vertex { position: [ test_icon.width as f32 / 2.0, -(test_icon.height as f32) / 2.0], color: [1.0, 1.0, 1.0], uv: [1.0, 1.0] },
            ], ());
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
            slice,
            pso,
            data,
        }
    }

    pub fn prepare(&mut self, _objtree: &ObjectTree, _map: &Map, _grid: Grid) {
    }

    pub fn paint(&mut self, _factory: &mut Factory, encoder: &mut Encoder, view: &RenderTargetView) {
        self.data.out = view.clone();

        let (x, y, _, _) = view.get_dimensions();

        // (0, 0) is the center of the screen, 1.0 = 1 pixel
        let zoom = 1.0;
        let transform = Transform {
            transform: [
                [2.0 / x as f32, 0.0, 0.0, 0.0],
                [0.0, 2.0 / y as f32, 0.0, 0.0],
                [0.0, 0.0, 1.0, 0.0],
                [0.0, 0.0, 0.0, 1.0 / zoom],
            ]
        };
        encoder.update_buffer(&self.data.transform, &[transform], 0).expect("update_buffer failed");
        encoder.draw(&self.slice, &self.pso, &self.data);
    }
}
