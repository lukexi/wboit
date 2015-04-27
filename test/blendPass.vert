#version 330 core

in vec3 aPosition;

out vec3 vPosition;

void main () {
    vPosition = aPosition;
    
    gl_Position = vec4(aPosition, 1.0);
}