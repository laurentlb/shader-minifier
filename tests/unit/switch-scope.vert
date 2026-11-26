#version 450

void main()
{
    int x = 0;

    switch (gl_VertexID)
    {
        case 0:
            x = 1;
            int y = 42;
            break;

        case 1:
            x = y;
            break;
    }

    gl_Position = vec4(float(x));
}
