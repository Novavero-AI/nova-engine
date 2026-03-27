#version 450

const float PI = 3.14159265359;

layout(set = 0, binding = 0) uniform FrameUBO {
    mat4 view;
    mat4 projection;
    vec4 cameraPos;
    vec4 lightDir;
    vec4 lightColor;
    mat4 cascadeMatrices[4];
    vec4 cascadeSplits;
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

layout(set = 1, binding = 0) uniform sampler2D texAlbedo;
layout(set = 1, binding = 1) uniform sampler2D texNormal;
layout(set = 1, binding = 2) uniform sampler2D texMetallicRoughness;
layout(set = 1, binding = 3) uniform sampler2D texAO;
layout(set = 1, binding = 4) uniform sampler2D texEmissive;

layout(set = 2, binding = 0) uniform sampler2DArrayShadow shadowMap;

layout(location = 0) in vec3 fragWorldPos;
layout(location = 1) in vec3 fragNormal;
layout(location = 2) in vec2 fragUV;
layout(location = 3) in vec4 fragColor;
layout(location = 4) in vec3 fragTangent;
layout(location = 5) in vec3 fragBitangent;

layout(location = 0) out vec4 outColor;

/* ---- Shadow sampling ---- */

float sampleShadow(vec3 worldPos) {
    /* View-space depth for cascade selection */
    float viewDepth = -(frame.view * vec4(worldPos, 1.0)).z;

    /* Select cascade */
    int cascade = 3;
    for (int i = 0; i < 4; i++) {
        if (viewDepth < frame.cascadeSplits[i]) {
            cascade = i;
            break;
        }
    }

    /* Project into light space */
    vec4 lsPos = frame.cascadeMatrices[cascade] * vec4(worldPos, 1.0);
    vec3 proj  = lsPos.xyz / lsPos.w;

    /* NDC [-1,1] XY -> UV [0,1] */
    vec2 shadowUV = proj.xy * 0.5 + 0.5;
    float depth   = proj.z;

    /* Out-of-bounds: fully lit */
    if (shadowUV.x < 0.0 || shadowUV.x > 1.0 ||
        shadowUV.y < 0.0 || shadowUV.y > 1.0 ||
        depth > 1.0) {
        return 1.0;
    }

    /* PCF 3x3 via hardware comparison sampler */
    float shadow = 0.0;
    vec2 texelSize = 1.0 / vec2(textureSize(shadowMap, 0).xy);
    for (int x = -1; x <= 1; x++) {
        for (int y = -1; y <= 1; y++) {
            vec2 offset = vec2(float(x), float(y)) * texelSize;
            shadow += texture(shadowMap,
                vec4(shadowUV + offset, float(cascade), depth));
        }
    }
    shadow /= 9.0;

    /* Cascade blending: smooth transition at split boundaries */
    float splitDist = frame.cascadeSplits[cascade];
    float blendStart = splitDist * 0.9;
    if (cascade < 3 && viewDepth > blendStart) {
        /* Sample next cascade */
        int next = cascade + 1;
        vec4 lsNext = frame.cascadeMatrices[next] * vec4(worldPos, 1.0);
        vec3 projNext = lsNext.xyz / lsNext.w;
        vec2 uvNext = projNext.xy * 0.5 + 0.5;
        float depthNext = projNext.z;

        float shadowNext = 0.0;
        for (int x = -1; x <= 1; x++) {
            for (int y = -1; y <= 1; y++) {
                vec2 offset = vec2(float(x), float(y)) * texelSize;
                shadowNext += texture(shadowMap,
                    vec4(uvNext + offset, float(next), depthNext));
            }
        }
        shadowNext /= 9.0;

        float blend = smoothstep(blendStart, splitDist, viewDepth);
        shadow = mix(shadow, shadowNext, blend);
    }

    return shadow;
}

/* ---- Cook-Torrance BRDF ---- */

/* GGX / Trowbridge-Reitz normal distribution. */
float distributionGGX(vec3 N, vec3 H, float a) {
    float a2    = a * a;
    float NdotH = max(dot(N, H), 0.0);
    float denom = NdotH * NdotH * (a2 - 1.0) + 1.0;
    return a2 / (PI * denom * denom);
}

/* Smith-Schlick geometry (single direction). */
float geometrySchlick(float NdotV, float k) {
    return NdotV / (NdotV * (1.0 - k) + k);
}

/* Smith geometry for both view and light. */
float geometrySmith(vec3 N, vec3 V, vec3 L, float roughness) {
    float r = roughness + 1.0;
    float k = (r * r) / 8.0;
    return geometrySchlick(max(dot(N, V), 0.0), k)
         * geometrySchlick(max(dot(N, L), 0.0), k);
}

/* Fresnel-Schlick approximation. */
vec3 fresnelSchlick(float cosTheta, vec3 F0) {
    return F0 + (1.0 - F0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), 5.0);
}

void main() {
    /* Sample textures */
    vec4 albedo    = texture(texAlbedo, fragUV) * pc.albedoFactor * fragColor;
    vec3 normalMap = texture(texNormal, fragUV).rgb * 2.0 - 1.0;
    vec2 mr        = texture(texMetallicRoughness, fragUV).bg;
    float ao       = mix(1.0, texture(texAO, fragUV).r, pc.aoStrength);
    vec3 emissive  = texture(texEmissive, fragUV).rgb
                   * pc.emissiveFactor.rgb * pc.emissiveFactor.a;

    float metallic  = mr.x * pc.metallic;
    float roughness = clamp(mr.y * pc.roughness, 0.04, 1.0);

    /* Normal mapping via TBN */
    mat3 TBN = mat3(normalize(fragTangent),
                     normalize(fragBitangent),
                     normalize(fragNormal));
    vec3 N = normalize(TBN * normalMap);

    vec3 V = normalize(frame.cameraPos.xyz - fragWorldPos);
    vec3 L = normalize(frame.lightDir.xyz);
    vec3 H = normalize(V + L);

    /* Cook-Torrance specular BRDF */
    vec3 F0 = mix(vec3(0.04), albedo.rgb, metallic);

    float D = distributionGGX(N, H, roughness);
    float G = geometrySmith(N, V, L, roughness);
    vec3  F = fresnelSchlick(max(dot(H, V), 0.0), F0);

    vec3 numerator  = D * G * F;
    float denom     = 4.0 * max(dot(N, V), 0.0)
                          * max(dot(N, L), 0.0) + 0.0001;
    vec3 specular   = numerator / denom;

    vec3 kD = (vec3(1.0) - F) * (1.0 - metallic);

    float NdotL   = max(dot(N, L), 0.0);
    vec3 radiance = frame.lightColor.rgb * frame.lightColor.a;
    float shadow  = sampleShadow(fragWorldPos);
    vec3 Lo       = (kD * albedo.rgb / PI + specular) * radiance * NdotL * shadow;

    /* Ambient (constant for now — IBL in a later phase) */
    vec3 ambient = vec3(0.03) * albedo.rgb * ao;

    outColor = vec4(ambient + Lo + emissive, albedo.a);
}
