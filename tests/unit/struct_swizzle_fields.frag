// Fields that look like swizzles are not renamed, the other fields are (#395).
struct Rect { float xy; float width; float zw; };

float area(Rect r) { return (r.zw - r.xy) * r.width; }

// Swizzle optimizations don't apply to these fields.
struct S { float x; float y; };
struct T { S m; vec2 v; };
uniform S s;
uniform T t;
uniform vec2 u;

vec2 combine() { return vec2(s.x, s.y); }
vec2 dropLast() { return vec2(u.y, s.x); }
vec2 nested() { return vec2(u.y, t.m.x); }
vec2 constructor() { return vec2(u.y, S(1., 2.).x); }

// Real swizzles are still optimized.
vec2 swizzles() { return vec2(u.x, u.y) + vec2(u.y, t.v.x); }
