use gfx;
use gfx::traits::FactoryExt;
use {Resources, Factory, Encoder, ColorFormat, RenderTargetView};

use dm::objtree::ObjectTree;
use dmm_tools::dmm::{Map, Grid};

gfx_defines! {
    vertex Vertex {
        position: [f32; 2] = "position",
        color: [f32; 3] = "color",
    }

    constant Transform {
        transform: [[f32; 4]; 4] = "transform",
    }

    pipeline pipe {
        vbuf: gfx::VertexBuffer<Vertex> = (),
        transform: gfx::ConstantBuffer<Transform> = "Transform",
        out: gfx::RenderTarget<ColorFormat> = "target",
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

        let (vertex_buffer, slice) = factory.create_vertex_buffer_with_slice(&[
            Vertex { position: [-0.5, -0.5], color: [0.0, 1.0, 0.0] },
            Vertex { position: [ 0.0,  0.5], color: [0.0, 0.0, 1.0] },
            Vertex { position: [ 0.5, -0.5], color: [1.0, 0.0, 0.0] },
        ], ());
        let transform_buffer = factory.create_constant_buffer(1);
        let data = pipe::Data {
            vbuf: vertex_buffer,
            transform: transform_buffer,
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
        encoder.update_buffer(&self.data.transform, &[TRANSFORM], 0);
        encoder.draw(&self.slice, &self.pso, &self.data);
    }
}
