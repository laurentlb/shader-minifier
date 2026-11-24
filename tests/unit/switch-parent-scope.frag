#version 450
uniform int sel;
out vec4 o;
void main() {
    int x=0, y=5;
    switch(sel){ case 0: int y=sel; y++; x=y; }
    o=vec4(x,y,0,1);
}
