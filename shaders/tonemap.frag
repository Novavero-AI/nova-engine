#version 450

layout(set = 0, binding = 0) uniform sampler2D hdrScene;
layout(set = 0, binding = 1) uniform sampler2D bloomTexture;

layout(push_constant) uniform PushConstants {
    vec2  inverseScreenSize;
    float bloomStrength;
    float _pad;
} pc;

layout(location = 0) in  vec2 fragUV;
layout(location = 0) out vec4 outColor;

/* ---- ACES filmic tonemapping (Narkowicz 2015 fit) ---- */

vec3 acesFilmic(vec3 x) {
    float a = 2.51;
    float b = 0.03;
    float c = 2.43;
    float d = 0.59;
    float e = 0.14;
    return clamp((x * (a * x + b)) / (x * (c * x + d) + e), 0.0, 1.0);
}

/* ---- Tonemapped sample (bloom composite + ACES) ---- */

vec3 sampleTonemapped(vec2 uv) {
    vec3 hdr   = texture(hdrScene, uv).rgb;
    vec3 bloom = texture(bloomTexture, uv).rgb;
    return acesFilmic(hdr + bloom * pc.bloomStrength);
    /* No manual gamma — swapchain is SRGB, hardware encodes. */
}

/* ---- FXAA 3.11 (Timothy Lottes, NVIDIA 2011) ---- */

float fxaaLuma(vec3 rgb) {
    return dot(rgb, vec3(0.299, 0.587, 0.114));
}

void main() {
    vec2 invSize = pc.inverseScreenSize;

    /* Sample tonemapped neighborhood for FXAA edge detection */
    vec3 rgbNW = sampleTonemapped(fragUV + vec2(-1.0, -1.0) * invSize);
    vec3 rgbNE = sampleTonemapped(fragUV + vec2( 1.0, -1.0) * invSize);
    vec3 rgbSW = sampleTonemapped(fragUV + vec2(-1.0,  1.0) * invSize);
    vec3 rgbSE = sampleTonemapped(fragUV + vec2( 1.0,  1.0) * invSize);
    vec3 rgbM  = sampleTonemapped(fragUV);

    float lumaNW = fxaaLuma(rgbNW);
    float lumaNE = fxaaLuma(rgbNE);
    float lumaSW = fxaaLuma(rgbSW);
    float lumaSE = fxaaLuma(rgbSE);
    float lumaM  = fxaaLuma(rgbM);

    float lumaMin = min(lumaM, min(min(lumaNW, lumaNE), min(lumaSW, lumaSE)));
    float lumaMax = max(lumaM, max(max(lumaNW, lumaNE), max(lumaSW, lumaSE)));

    /* Edge direction */
    vec2 dir;
    dir.x = -((lumaNW + lumaNE) - (lumaSW + lumaSE));
    dir.y =  ((lumaNW + lumaSW) - (lumaNE + lumaSE));

    float FXAA_REDUCE_MUL = 1.0 / 8.0;
    float FXAA_REDUCE_MIN = 1.0 / 128.0;
    float FXAA_SPAN_MAX   = 8.0;

    float dirReduce = max(
        (lumaNW + lumaNE + lumaSW + lumaSE) * 0.25 * FXAA_REDUCE_MUL,
        FXAA_REDUCE_MIN);
    float rcpDirMin = 1.0 / (min(abs(dir.x), abs(dir.y)) + dirReduce);
    dir = min(vec2(FXAA_SPAN_MAX), max(vec2(-FXAA_SPAN_MAX),
              dir * rcpDirMin)) * invSize;

    /* Two-tap blend along edge */
    vec3 rgbA = 0.5 * (
        sampleTonemapped(fragUV + dir * (1.0/3.0 - 0.5)) +
        sampleTonemapped(fragUV + dir * (2.0/3.0 - 0.5)));

    /* Four-tap blend for higher quality */
    vec3 rgbB = rgbA * 0.5 + 0.25 * (
        sampleTonemapped(fragUV + dir * -0.5) +
        sampleTonemapped(fragUV + dir *  0.5));

    float lumaB = fxaaLuma(rgbB);

    /* If four-tap went out of range, fall back to two-tap */
    if (lumaB < lumaMin || lumaB > lumaMax) {
        outColor = vec4(rgbA, 1.0);
    } else {
        outColor = vec4(rgbB, 1.0);
    }
}
