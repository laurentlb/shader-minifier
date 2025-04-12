#version 300

// Argument inlining inlines the argument of a function call into the function body.

float noinline_canInlineWhenResolvable(float a) { return a+10.; }
float noinline_cannotInlineWhenNotResolvable(float a) { return a+11.; }
float noinline_cannotInlineWhenNotResolvable(vec2 a) { return length(a)+11.; }

float noinline_canInlineWhenInParameter(in float a) { return a+20.; }
float noinline_cannotInlineWhenOutParameter(out float a) { return a+21.; }
float noinline_cannotInlineWhenInOutParameter(inout float a) { return a+22.; }

float noinline_canInlineWhenArgIsAlwaysTheSame(float a) { return a+30.; }
float noinline_cannotInlineWhenArgsAreDifferent(float a) { return a+31.; }

float noinline_canInlineWhenArgIsInlinable1(bool a) { return a?20.:40.; }
float noinline_canInlineWhenArgIsInlinable2(int a) { return float(a)+40.; }
float noinline_canInlineWhenArgIsInlinable3(float a) { return a+40.; }
float noinline_canInlineWhenArgIsInlinable4(vec3 a) { return a.x; }
float noinline_canInlineWhenArgIsInlinable5(float a) { return a+40.; }
float noinline_canInlineWhenArgIsInlinable6(float a) { return a+40.; }
float noinline_cannotInlineWhenArgIsNotInlinable(float a) { return a+41.; }

float f()
{
	float s = 0.;
	
	s += s + noinline_canInlineWhenResolvable(1.);
	s += s + noinline_cannotInlineWhenNotResolvable(1.);
	
	s += s + noinline_canInlineWhenInParameter(1.);
	s += s + noinline_cannotInlineWhenOutParameter(s);
	s += s + noinline_cannotInlineWhenInOutParameter(s);
	
	s += s + noinline_canInlineWhenArgIsAlwaysTheSame(1.);
	s += s + noinline_cannotInlineWhenArgsAreDifferent(1.);
	s += s + noinline_cannotInlineWhenArgsAreDifferent(2.);

	s += s + noinline_canInlineWhenArgIsInlinable1(true);
	s += s + noinline_canInlineWhenArgIsInlinable2(-18);
	s += s + noinline_canInlineWhenArgIsInlinable3(456.0);
	s += s + noinline_canInlineWhenArgIsInlinable4(vec3(9));
	s += s + noinline_canInlineWhenArgIsInlinable5(acos(-1.));
	s += s + noinline_canInlineWhenArgIsInlinable6(1./3.);
	s += s + noinline_cannotInlineWhenArgIsNotInlinable(s);
	s += s + noinline_cannotInlineWhenArgIsNotInlinable(acos(s));
	
	return s;
}


// sampler types are immutable and cannot be locals, they must always be inlined (#496)
out vec4 fragColor;
uniform sampler2D samp;
vec3 dof(sampler2D tex, float f) {
        for (;;) {
                vec3 a = texture(tex,vec2(0)).rgb;
                return a.zyx+a.xyz*(f*=f);
        }
}


void main()
{
	f();
	fragColor.xyz = dof(samp, 10.0);
}
