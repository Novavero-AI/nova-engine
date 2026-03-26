#version 450

layout(set = 0, binding = 0) uniform sampler2D texSampler;

layout(location = 0) in vec3 fragNormal;
layout(location = 1) in vec2 fragUV;
layout(location = 2) in vec4 fragColor;

layout(location = 0) out vec4 outColor;

void main() {
    vec4 texColor = texture(texSampler, fragUV);
    vec3 lightDir = normalize(vec3(0.5, 1.0, 0.3));
    float ndotl   = max(dot(normalize(fragNormal), lightDir), 0.0);
    vec3 ambient  = vec3(0.15);
    vec3 lit      = texColor.rgb * fragColor.rgb * (ambient + ndotl);
    outColor      = vec4(lit, texColor.a * fragColor.a);
}
