float4 main() : SV_TARGET {
    float a = 1.0;
    float b = 2.0;
    float c = 3.0;

    float r1 = true ? (a, b) : c;
    float r2 = true ? a, b : c;
    float r3 = false ? a : (b, c);

    float counter = 0.0;
    float r4 = true
        ? (counter += 1.0, counter += 2.0)
        : (counter += 3.0);

    return float4(r1, r2, r3, r4);
}
