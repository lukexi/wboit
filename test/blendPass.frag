#version 330 core

uniform sampler2D accumTexture;
uniform sampler2D revealageTexture;

in vec3 vPosition;

out vec4 color;

void main() {
    vec4 accum   = texture(accumTexture    , vPosition.xy / 2 + 0.5, 0);
    float reveal = texture(revealageTexture, vPosition.xy / 2 + 0.5, 0).r;
     
    // Blend Func: GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA
    color = vec4(accum.rgb / max(accum.a, 1e-5), reveal);
    // color = vec4(reveal);
}

