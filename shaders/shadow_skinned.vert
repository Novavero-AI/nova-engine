#version 450

layout(set = 0, binding = 0) readonly buffer BoneSSBO {
    mat4 bones[128];
} skin;

layout(push_constant) uniform PushConstants {
    mat4 lightVP;
    mat4 model;
} pc;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec2 inUV;
layout(location = 3) in vec4 inTangent;
layout(location = 4) in vec4 inBoneIndices;
layout(location = 5) in vec4 inBoneWeights;

void main() {
    ivec4 idx = ivec4(inBoneIndices);
    vec4 w = inBoneWeights;

    mat4 skinMatrix = w.x * skin.bones[idx.x]
                    + w.y * skin.bones[idx.y]
                    + w.z * skin.bones[idx.z]
                    + w.w * skin.bones[idx.w];

    gl_Position = pc.lightVP * pc.model * skinMatrix * vec4(inPosition, 1.0);
}
