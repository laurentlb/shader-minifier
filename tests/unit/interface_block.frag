uniform MyBlock
{
  vec3 color; // treated as a global variable
  float alpha;
};

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    fragColor = vec4(color, alpha);
}
