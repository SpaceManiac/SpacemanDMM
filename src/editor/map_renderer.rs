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
        out: gfx::BlendTarget<ColorFormat> = ("target", gfx::state::ColorMask::all(), gfx::preset::blend::ALPHA),
    }
}

pub struct GliumTest {
    slice: gfx::Slice<Resources>,
    pso: gfx::PipelineState<Resources, pipe::Meta>,
    data: pipe::Data<Resources>,
}

impl GliumTest {
    pub fn new(factory: &mut Factory, view: &RenderTargetView) -> GliumTest {
        let pso = factory.create_pipeline_simple(
            include_bytes!("shaders/main_150.glslv"),
            include_bytes!("shaders/main_150.glslf"),
            pipe::new()
        ).expect("create_pipeline_simple failed");

        let test_icon = IconFile::from_file(factory, "icons/obj/device.dmi".as_ref())
            .expect("IconFile::from_file").texture;
        let sampler = factory.create_sampler(gfx::texture::SamplerInfo::new(
            gfx::texture::FilterMethod::Scale,
            gfx::texture::WrapMode::Clamp));

        let (vertex_buffer, slice) = factory.create_vertex_buffer_with_slice(&[
            Vertex { position: [-1.0, -1.0], color: [1.0, 1.0, 1.0], uv: [0.0, 1.0] },
            Vertex { position: [ 0.0,  1.0], color: [1.0, 1.0, 1.0], uv: [0.5, 0.0] },
            Vertex { position: [ 1.0, -1.0], color: [1.0, 1.0, 1.0], uv: [1.0, 1.0] },
        ], ());
        let transform_buffer = factory.create_constant_buffer(1);
        let data = pipe::Data {
            vbuf: vertex_buffer,
            transform: transform_buffer,
            tex: (test_icon, sampler),
            out: view.clone(),
        };

        GliumTest {
            slice,
            pso,
            data,
        }
    }

    pub fn prepare(&mut self, _objtree: &ObjectTree, _map: &Map, _grid: Grid) {
    }

    pub fn paint(&mut self, _factory: &mut Factory, encoder: &mut Encoder) {
        const TRANSFORM: Transform = Transform {
            transform: [
                [1.0, 0.0, 0.0, 0.0],
                [0.0, 1.0, 0.0, 0.0],
                [0.0, 0.0, 1.0, 0.0],
                [0.0, 0.0, 0.0, 1.0f32],
            ]
        };
        encoder.update_buffer(&self.data.transform, &[TRANSFORM], 0).expect("update_buffer failed");
        encoder.draw(&self.slice, &self.pso, &self.data);
    }
}
