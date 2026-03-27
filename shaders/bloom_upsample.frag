#version 450

layout(set = 0, binding = 0) uniform sampler2D srcTexture;

layout(push_constant) uniform PushConstants {
    vec2  texelSize;
    float currentMip;
    float _pad;
} pc;

layout(location = 0) in  vec2 fragUV;
layout(location = 0) out vec4 outColor;

void main() {
    /* 9-tap tent filter (3x3, weighted as a tent).
     * From Jimenez 2014. Produces smooth upsampling.
     * Additive blend is handled by pipeline blend state (ONE, ONE). */
    vec2 ts = pc.texelSize;

    vec3 s;
    s  = texture(srcTexture, fragUV + vec2(-1.0, -1.0) * ts).rgb * 1.0;
    s += texture(srcTexture, fragUV + vec2( 0.0, -1.0) * ts).rgb * 2.0;
    s += texture(srcTexture, fragUV + vec2( 1.0, -1.0) * ts).rgb * 1.0;
    s += texture(srcTexture, fragUV + vec2(-1.0,  0.0) * ts).rgb * 2.0;
    s += texture(srcTexture, fragUV                          ).rgb * 4.0;
    s += texture(srcTexture, fragUV + vec2( 1.0,  0.0) * ts).rgb * 2.0;
    s += texture(srcTexture, fragUV + vec2(-1.0,  1.0) * ts).rgb * 1.0;
    s += texture(srcTexture, fragUV + vec2( 0.0,  1.0) * ts).rgb * 2.0;
    s += texture(srcTexture, fragUV + vec2( 1.0,  1.0) * ts).rgb * 1.0;

    outColor = vec4(s / 16.0, 1.0);
}
