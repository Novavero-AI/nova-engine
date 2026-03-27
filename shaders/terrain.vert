#version 450

layout(set = 0, binding = 0) uniform FrameUBO {
    mat4 view;
    mat4 projection;
    vec4 cameraPos;
    vec4 lightDir;
    vec4 lightColor;
    mat4 cascadeMatrices[4];
    vec4 cascadeSplits;
} frame;

layout(set = 1, binding = 0) uniform sampler2D heightMap;

layout(push_constant) uniform PushConstants {
    mat4  model;
    vec4  terrainParams;  /* x=heightScale, y=texelSize, z=uvScale, w=0 */
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

void main() {
    float heightScale = pc.terrainParams.x;
    float texelSize   = pc.terrainParams.y;
    float uvScale     = pc.terrainParams.z;

    vec2 uv = inUV;

    /* Sample height and displace Y */
    float h = texture(heightMap, uv).r * heightScale;
    vec3 displaced = vec3(inPosition.x, h, inPosition.z);

    /* Compute normal from central differences */
    float hL = texture(heightMap, uv + vec2(-texelSize, 0.0)).r * heightScale;
    float hR = texture(heightMap, uv + vec2( texelSize, 0.0)).r * heightScale;
    float hD = texture(heightMap, uv + vec2(0.0, -texelSize)).r * heightScale;
    float hU = texture(heightMap, uv + vec2(0.0,  texelSize)).r * heightScale;
    vec3 normal = normalize(vec3(hL - hR, 2.0 * texelSize, hD - hU));

    /* World transform */
    vec4 worldPos = pc.model * vec4(displaced, 1.0);
    gl_Position = frame.projection * frame.view * worldPos;

    fragWorldPos = worldPos.xyz;
    fragNormal   = normalize(mat3(pc.model) * normal);
    fragUV       = uv * uvScale;
    fragColor    = inColor;
}
