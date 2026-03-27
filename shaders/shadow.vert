#version 450

layout(push_constant) uniform PushConstants {
    mat4 lightVP;   /* offset 0: light view-projection for this cascade */
    mat4 model;     /* offset 64: per-object model matrix */
} pc;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec2 inUV;
layout(location = 3) in vec4 inTangent;
layout(location = 4) in vec4 inColor;

void main() {
    gl_Position = pc.lightVP * pc.model * vec4(inPosition, 1.0);
}
