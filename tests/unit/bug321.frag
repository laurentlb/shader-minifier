float n;
const float s = 1.;
float a(float s)
{
    s = 3.;
    n = 2.;
    return s;
}
float b()
{
    float t = 3.;
    t -= s;
    return a(s)+a(5.);
}
