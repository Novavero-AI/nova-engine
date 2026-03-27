#version 450

layout(set = 0, binding = 0) uniform sampler2D srcTexture;

layout(push_constant) uniform PushConstants {
    vec2  texelSize;
    float currentMip;
    float _pad;
} pc;

layout(location = 0) in  vec2 fragUV;
layout(location = 0) out vec4 outColor;

/* Karis average: weight by 1/(1+luma) to suppress fireflies
 * on the first downsample from HDR (Karis 2013). */
float karisWeight(vec3 c) {
    float luma = dot(c, vec3(0.2126, 0.7152, 0.0722));
    return 1.0 / (1.0 + luma);
}

void main() {
    vec2 uv = fragUV;
    vec2 ts = pc.texelSize;

    if (pc.currentMip == 0.0) {
        /* First mip: 13-tap Karis-weighted downsample
         * (Jimenez 2014, "Next Gen Post Processing in CoD:AW"). */
        vec3 a = texture(srcTexture, uv + vec2(-2.0, -2.0) * ts).rgb;
        vec3 b = texture(srcTexture, uv + vec2( 0.0, -2.0) * ts).rgb;
        vec3 c = texture(srcTexture, uv + vec2( 2.0, -2.0) * ts).rgb;
        vec3 d = texture(srcTexture, uv + vec2(-2.0,  0.0) * ts).rgb;
        vec3 e = texture(srcTexture, uv                          ).rgb;
        vec3 f = texture(srcTexture, uv + vec2( 2.0,  0.0) * ts).rgb;
        vec3 g = texture(srcTexture, uv + vec2(-2.0,  2.0) * ts).rgb;
        vec3 h = texture(srcTexture, uv + vec2( 0.0,  2.0) * ts).rgb;
        vec3 i = texture(srcTexture, uv + vec2( 2.0,  2.0) * ts).rgb;
        vec3 j = texture(srcTexture, uv + vec2(-1.0, -1.0) * ts).rgb;
        vec3 k = texture(srcTexture, uv + vec2( 1.0, -1.0) * ts).rgb;
        vec3 l = texture(srcTexture, uv + vec2(-1.0,  1.0) * ts).rgb;
        vec3 m = texture(srcTexture, uv + vec2( 1.0,  1.0) * ts).rgb;

        vec3 g0 = (a + b + d + e) * 0.25;
        vec3 g1 = (b + c + e + f) * 0.25;
        vec3 g2 = (d + e + g + h) * 0.25;
        vec3 g3 = (e + f + h + i) * 0.25;
        vec3 g4 = (j + k + l + m) * 0.25;

        float w0 = karisWeight(g0);
        float w1 = karisWeight(g1);
        float w2 = karisWeight(g2);
        float w3 = karisWeight(g3);
        float w4 = karisWeight(g4);

        float wSum = w0 + w1 + w2 + w3 + w4;
        outColor = vec4((g0*w0 + g1*w1 + g2*w2 + g3*w3 + g4*w4) / wSum, 1.0);
    } else {
        /* Subsequent mips: bilinear 4-tap box filter. */
        vec3 s;
        s  = texture(srcTexture, uv + vec2(-1.0, -1.0) * ts).rgb;
        s += texture(srcTexture, uv + vec2( 1.0, -1.0) * ts).rgb;
        s += texture(srcTexture, uv + vec2(-1.0,  1.0) * ts).rgb;
        s += texture(srcTexture, uv + vec2( 1.0,  1.0) * ts).rgb;
        outColor = vec4(s * 0.25, 1.0);
    }
}
