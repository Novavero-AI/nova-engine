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

/* Splatmap: R=layer0, G=layer1, B=layer2, A=layer3 */
layout(set = 1, binding = 1) uniform sampler2D splatMap;

/* Material layers (tiled textures) */
layout(set = 1, binding = 2) uniform sampler2D layer0;  /* e.g. grass */
layout(set = 1, binding = 3) uniform sampler2D layer1;  /* e.g. rock */
layout(set = 1, binding = 4) uniform sampler2D layer2;  /* e.g. dirt */
layout(set = 1, binding = 5) uniform sampler2D layer3;  /* e.g. snow */

/* Shadow map */
layout(set = 2, binding = 0) uniform sampler2DArrayShadow shadowMap;

layout(location = 0) in vec3 fragWorldPos;
layout(location = 1) in vec3 fragNormal;
layout(location = 2) in vec2 fragUV;
layout(location = 3) in vec4 fragColor;

layout(location = 0) out vec4 outColor;

/* Shadow sampling (same as pbr.frag) */
float sampleShadow(vec3 worldPos) {
    float viewDepth = -(frame.view * vec4(worldPos, 1.0)).z;

    int cascade = 3;
    for (int i = 0; i < 4; i++) {
        if (viewDepth < frame.cascadeSplits[i]) {
            cascade = i;
            break;
        }
    }

    vec4 lsPos = frame.cascadeMatrices[cascade] * vec4(worldPos, 1.0);
    vec3 proj  = lsPos.xyz / lsPos.w;
    vec2 shadowUV = proj.xy * 0.5 + 0.5;
    float depth   = proj.z;

    if (shadowUV.x < 0.0 || shadowUV.x > 1.0 ||
        shadowUV.y < 0.0 || shadowUV.y > 1.0 ||
        depth > 1.0) {
        return 1.0;
    }

    float shadow = 0.0;
    vec2 texelSize = 1.0 / vec2(textureSize(shadowMap, 0).xy);
    for (int x = -1; x <= 1; x++) {
        for (int y = -1; y <= 1; y++) {
            shadow += texture(shadowMap,
                vec4(shadowUV + vec2(float(x), float(y)) * texelSize,
                     float(cascade), depth));
        }
    }
    return shadow / 9.0;
}

void main() {
    /* Splatmap: use fragUV for both splatmap and tiled layers.
     * Artist should set splatmap sampler to clamp-to-edge and
     * material layer samplers to repeat. */
    vec4 weights = texture(splatMap, fragUV);
    float wSum = weights.r + weights.g + weights.b + weights.a;
    if (wSum > 0.0) weights /= wSum;

    /* Sample tiled material layers */
    vec3 c0 = texture(layer0, fragUV).rgb;
    vec3 c1 = texture(layer1, fragUV).rgb;
    vec3 c2 = texture(layer2, fragUV).rgb;
    vec3 c3 = texture(layer3, fragUV).rgb;

    vec3 albedo = c0 * weights.r + c1 * weights.g
                + c2 * weights.b + c3 * weights.a;

    /* Simple directional lighting (terrain doesn't need full PBR) */
    vec3 N = normalize(fragNormal);
    vec3 L = normalize(frame.lightDir.xyz);
    float NdotL = max(dot(N, L), 0.0);

    vec3 radiance = frame.lightColor.rgb * frame.lightColor.a;
    float shadow = sampleShadow(fragWorldPos);
    vec3 lit = albedo * (vec3(0.15) + radiance * NdotL * shadow);

    outColor = vec4(lit, 1.0);
}
