#version 450

layout(set = 0, binding = 0) uniform FrameUBO {
    mat4 view;
    mat4 projection;
    vec4 cameraPos;
    vec4 lightDir;
    vec4 lightColor;
} frame;

layout(push_constant) uniform PushConstants {
    mat4 model;
    vec4 albedoFactor;
    vec4 emissiveFactor;
    float metallic;
    float roughness;
    float aoStrength;
    float _pad;
    vec2 uvScale;
    vec2 uvOffset;
} pc;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec2 inUV;
layout(location = 3) in vec4 inTangent;
layout(location = 4) in vec4 inColor;

layout(location = 0) out vec3 fragWorldPos;
layout(location = 1) out vec3 fragNormal;
layout(location = 2) out vec2 fragUV;
layout(location = 3) out vec4 fragColor;
layout(location = 4) out vec3 fragTangent;
layout(location = 5) out vec3 fragBitangent;

void main() {
    vec4 worldPos = pc.model * vec4(inPosition, 1.0);
    gl_Position = frame.projection * frame.view * worldPos;

    mat3 normalMatrix = transpose(inverse(mat3(pc.model)));
    vec3 N = normalize(normalMatrix * inNormal);
    vec3 T = normalize(normalMatrix * inTangent.xyz);
    T = normalize(T - dot(T, N) * N);
    vec3 B = cross(N, T) * inTangent.w;

    fragWorldPos  = worldPos.xyz;
    fragNormal    = N;
    fragUV        = inUV * pc.uvScale + pc.uvOffset;
    fragColor     = inColor;
    fragTangent   = T;
    fragBitangent = B;
}
