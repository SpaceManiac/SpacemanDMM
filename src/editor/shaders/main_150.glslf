#version 150 core

uniform sampler2D tex;

in vec4 v_color;
in vec2 v_uv;

out vec4 target;

void main() {
    target = v_color * texture(tex, v_uv);
}
