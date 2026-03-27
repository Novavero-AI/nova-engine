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

layout(set = 1, binding = 0) readonly buffer BoneSSBO {
    mat4 bones[128];
} skin;

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
layout(location = 4) in vec4 inBoneIndices;
layout(location = 5) in vec4 inBoneWeights;

layout(location = 0) out vec3 fragWorldPos;
layout(location = 1) out vec3 fragNormal;
layout(location = 2) out vec2 fragUV;
layout(location = 3) out vec4 fragColor;
layout(location = 4) out vec3 fragTangent;
layout(location = 5) out vec3 fragBitangent;

void main() {
    /* Linear blend skinning */
    ivec4 idx = ivec4(inBoneIndices);
    vec4 w = inBoneWeights;

    mat4 skinMatrix = w.x * skin.bones[idx.x]
                    + w.y * skin.bones[idx.y]
                    + w.z * skin.bones[idx.z]
                    + w.w * skin.bones[idx.w];

    vec4 skinnedPos    = skinMatrix * vec4(inPosition, 1.0);
    vec3 skinnedNormal = mat3(skinMatrix) * inNormal;
    vec3 skinnedTangent = mat3(skinMatrix) * inTangent.xyz;

    /* World transform */
    vec4 worldPos = pc.model * skinnedPos;
    gl_Position = frame.projection * frame.view * worldPos;

    mat3 normalMatrix = transpose(inverse(mat3(pc.model)));
    vec3 N = normalize(normalMatrix * skinnedNormal);
    vec3 T = normalize(normalMatrix * skinnedTangent);
    T = normalize(T - dot(T, N) * N);
    vec3 B = cross(N, T) * inTangent.w;

    fragWorldPos  = worldPos.xyz;
    fragNormal    = N;
    fragUV        = inUV * pc.uvScale + pc.uvOffset;
    fragColor     = vec4(1.0);
    fragTangent   = T;
    fragBitangent = B;
}
