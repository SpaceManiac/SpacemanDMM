#version 150 core

in vec4 position;
in vec4 color;
in vec2 uv;

uniform Transform {
    mat4 transform;
};

out vec4 v_color;
out vec2 v_uv;

void main() {
    v_uv = uv;
    v_color = color;
    gl_Position = position * transform;
}
