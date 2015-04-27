#version 330 core

uniform mat4 uMVP;

in vec3 aPosition;

out vec3 vPosition;

void main( void ) {
    vPosition = aPosition;
    gl_Position = uMVP * vec4( aPosition , 1.0 );
}