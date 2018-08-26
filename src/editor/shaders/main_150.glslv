#version 150 core

in vec4 position;
in vec3 color;

uniform Transform {
    mat4 transform;
};

out vec4 v_color;

void main() {
    v_color = vec4(color, 1.0);
    gl_Position = position * transform;
}
