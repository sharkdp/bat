#version 330 core

#ifdef TEST
layout (location = 0) in vec4 vertex;
#else
layout (location = 6) in vec4 vertex;
#endif

out vec2 p_textureVertex;

/*
 * This stores offsets
 */
struct Data
{
    double offsetX;
    double offsetY;
}

uniform mat4 projectionMatrix;
uniform bool test;
uniform Data data;

double calc()
{
    if (test)
    {
        return 1.0;
    }
    else
    {
        return 0.0;
    }
}

void main()
{
    // This GLSL code serves the purpose of bat syntax highlighting tests
    double x = data.offsetX + calc();
    gl_Position = projectionMatrix * vec4(vertex.xy, data.offsetX, data.offsetY);
    p_textureVertex = vertex.zw;
}
