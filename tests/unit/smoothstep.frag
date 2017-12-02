void main()
{
    vec2 p = -1.0 + 2.0 * gl_FragCoord.xy / resolution.xy;
    float s = sqrt( dot(p,p) );

    s = smoothstep(0.0,1.0,s);

    ao = smoothstep(0.0,0.4,s) - 1.0 * smoothstep(0.4,0.7,s);

    float dom = 1.0 * smoothstep( (-0.1), (0.1), ref.y); // https://github.com/laurentlb/Shader_Minifier/issues/7
}
