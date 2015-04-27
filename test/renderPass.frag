#version 330 core

uniform vec4 diffuseColor;

in vec3 vPosition;

layout (location = 0) out vec4 accumBuffer;
layout (location = 1) out vec4 revealageBuffer;

void main () {

    vec4 color = diffuseColor; // regular shading code

    // NOTE: Output linear (not gamma encoded!), unmultiplied color from
    // the rest of the shader.  
     
    // Insert your favorite weighting function here. The color-based factor
    // avoids color pollution from the edges of wispy clouds. The z-based
    // factor gives precedence to nearer surfaces.
    float term1 = max(min(1.0, max(max(color.r, color.g), color.b) * color.a), color.a);
    float term2 = clamp(0.03 / (1e-5 + pow(vPosition.z / 200, 4.0)), 1e-2, 3e3);
    float weight = term1 * term2;
    
    // Blend Func: GL_ONE, GL_ONE -- Luke sez: he means "specify these outside" (see topmost snippet above^^^)
    // Switch to premultiplied alpha and weight
    accumBuffer = vec4(color.rgb * color.a, color.a) * weight;
     
    // Blend Func: GL_ZERO, GL_ONE_MINUS_SRC_ALPHA
    revealageBuffer.a = color.a;
}