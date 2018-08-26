use gfx::{self, Factory, Encoder};

use dm::objtree::ObjectTree;
use dmm_tools::dmm::{Map, Grid};

gfx_vertex_struct!(Vertex {
    position: [f32; 2] = "position",
    color: [f32; 3] = "color",
});

pub struct GliumTest {
    /*vertex_buffer: glium::VertexBuffer<Vertex>,
    index_buffer: glium::IndexBuffer<u16>,
    program: glium::Program,*/
}

impl GliumTest {
    pub fn new() -> GliumTest {
        /*use glium::index::PrimitiveType;

        let vertex_buffer = glium::VertexBuffer::new(display, &[
            Vertex { position: [-0.5, -0.5], color: [0.0, 1.0, 0.0] },
            Vertex { position: [ 0.0,  0.5], color: [0.0, 0.0, 1.0] },
            Vertex { position: [ 0.5, -0.5], color: [1.0, 0.0, 0.0] },
        ]).unwrap();

        // building the index buffer
        let index_buffer = glium::IndexBuffer::new(
            display, PrimitiveType::TrianglesList,
            &[0u16, 1, 2]).unwrap();

        // compiling shaders and linking them together
        let program = program!(display,
            140 => {
                vertex: "
                    #version 140
                    uniform mat4 matrix;
                    in vec2 position;
                    in vec3 color;
                    out vec3 vColor;
                    void main() {
                        gl_Position = vec4(position, 0.0, 1.0) * matrix;
                        vColor = color;
                    }
                ",

                fragment: "
                    #version 140
                    in vec3 vColor;
                    out vec4 f_color;
                    void main() {
                        f_color = vec4(vColor, 1.0);
                    }
                "
            },

            110 => {
                vertex: "
                    #version 110
                    uniform mat4 matrix;
                    attribute vec2 position;
                    attribute vec3 color;
                    varying vec3 vColor;
                    void main() {
                        gl_Position = vec4(position, 0.0, 1.0) * matrix;
                        vColor = color;
                    }
                ",

                fragment: "
                    #version 110
                    varying vec3 vColor;
                    void main() {
                        gl_FragColor = vec4(vColor, 1.0);
                    }
                ",
            },

            100 => {
                vertex: "
                    #version 100
                    uniform lowp mat4 matrix;
                    attribute lowp vec2 position;
                    attribute lowp vec3 color;
                    varying lowp vec3 vColor;
                    void main() {
                        gl_Position = vec4(position, 0.0, 1.0) * matrix;
                        vColor = color;
                    }
                ",

                fragment: "
                    #version 100
                    varying lowp vec3 vColor;
                    void main() {
                        gl_FragColor = vec4(vColor, 1.0);
                    }
                ",
            },
        ).unwrap();

        GliumTest {
            vertex_buffer, index_buffer, program
        }*/
        GliumTest {}
    }

    pub fn prepare(&mut self, _objtree: &ObjectTree, _map: &Map, _grid: Grid) {
    }

    /*
    pub fn paint<F: Factory<()>>(&mut self, factory: &mut F, encoder: &mut Encoder) {
        // building the uniforms
        let uniforms = uniform! {
            matrix: [
                [1.0, 0.0, 0.0, 0.0],
                [0.0, 1.0, 0.0, 0.0],
                [0.0, 0.0, 1.0, 0.0],
                [0.0, 0.0, 0.0, 1.0f32]
            ]
        };

        // drawing a frame
        target.draw(&self.vertex_buffer, &self.index_buffer, &self.program, &uniforms, &Default::default()).unwrap();
    }
    */
}
