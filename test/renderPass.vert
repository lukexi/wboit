#version 330 core

uniform mat4 uMVP;

in vec3 aPosition;

out vec3 vPosition;

void main(void) {
    vec4 transformedPosition = uMVP * vec4( aPosition , 1.0 );
    vPosition = transformedPosition.xyz;
    gl_Position = transformedPosition;
}