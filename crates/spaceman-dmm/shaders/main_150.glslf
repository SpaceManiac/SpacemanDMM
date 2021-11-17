#version 150 core

uniform sampler2D tex;

in vec4 v_color;
in vec2 v_uv;

out vec4 Target0;

void main() {
    Target0 = v_color * texture(tex, v_uv);
}
