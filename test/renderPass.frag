#version 330 core

uniform vec4 uDiffuseColor;

in vec3 vPosition;

layout (location = 0) out vec4 accumBuffer;
layout (location = 1) out vec4 revealageBuffer;

void main () {

    vec4 color = uDiffuseColor; // regular shading code
    // color = vec4(0,0,1,1);

    // NOTE: Output linear (not gamma encoded!), unmultiplied color from
    // the rest of the shader.  
     
    // Insert your favorite weighting function here. The color-based factor
    // avoids color pollution from the edges of wispy clouds. The z-based
    // factor gives precedence to nearer surfaces.
    float weight = max(min(1.0, max(max(color.r, color.g), color.b) * color.a), color.a) 
                 * clamp(0.03 / (1e-5 + pow(vPosition.z / 200, 4.0)), 1e-2, 3e3);
    
    // Blend Func: GL_ONE, GL_ONE
    // Switch to premultiplied alpha and weight
    accumBuffer = vec4(color.rgb * color.a, color.a) * weight;
     
    // Blend Func: GL_ZERO, GL_ONE_MINUS_SRC_ALPHA
    revealageBuffer = vec4(color.a);
}