// Fields that look like swizzles are not renamed, the other fields are (#395).
struct Rect { float xy; float width; float zw; };

float area(Rect r) { return (r.zw - r.xy) * r.width; }
