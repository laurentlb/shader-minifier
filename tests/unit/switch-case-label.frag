#version 450
uniform int sel;
out vec4 color;
const ivec2 V = ivec2(1, 5);

// #617: a case label can refer to a const declared in an earlier case.
void main()
{
    switch (sel) {
    case 0:
        const int size = V.y;
        color = vec4(size);
        break;
    case size:
        color = vec4(1);
        break;
    }
}
