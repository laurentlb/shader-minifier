// Argument inlining inlines the argument of a function call into the function body.

float noinline_canInlineWhenResolvable(float a) { return a+10.; }
float noinline_cannotInlineWhenNotResolvable(float a) { return a+11.; }
float noinline_cannotInlineWhenNotResolvable(vec2 a) { return a+11.; }

float noinline_canInlineWhenInParameter(in float a) { return a+20.; }
float noinline_cannotInlineWhenOutParameter(out float a) { return a+21.; }
float noinline_cannotInlineWhenInOutParameter(inout float a) { return a+22.; }

float noinline_canInlineWhenArgIsAlwaysTheSame(float a) { return a+30.; }
float noinline_cannotInlineWhenArgsAreDifferent(float a) { return a+31.; }

float noinline_canInlineWhenArgIsInlinable1(float a) { return a+40.; }
float noinline_canInlineWhenArgIsInlinable2(float a) { return a+40.; }
float noinline_canInlineWhenArgIsInlinable3(float a) { return a+40.; }
float noinline_canInlineWhenArgIsInlinable4(float a) { return a+40.; }
float noinline_canInlineWhenArgIsInlinable5(float a) { return a+40.; }
float noinline_canInlineWhenArgIsInlinable6(float a) { return a+40.; }
float noinline_cannotInlineWhenArgIsNotInlinable(float a) { return a+41.; }

float f()
{
	float s = 0;
	
	s += noinline_canInlineWhenResolvable(1.);
	s += noinline_cannotInlineWhenNotResolvable(1.);
	
	s += noinline_canInlineWhenInParameter(1.);
	s += noinline_cannotInlineWhenOutParameter(s);
	s += noinline_cannotInlineWhenInOutParameter(s);
	
	s += noinline_canInlineWhenArgIsAlwaysTheSame(1.);
	s += noinline_cannotInlineWhenArgsAreDifferent(1.);
	s += noinline_cannotInlineWhenArgsAreDifferent(2.);

	s += noinline_canInlineWhenArgIsInlinable1(true);
	s += noinline_canInlineWhenArgIsInlinable2(-18);
	s += noinline_canInlineWhenArgIsInlinable3(456.0);
	s += noinline_canInlineWhenArgIsInlinable4(vec3(9));
	s += noinline_canInlineWhenArgIsInlinable5(acos(-1.));
	s += noinline_canInlineWhenArgIsInlinable6(1./3.);
	s += noinline_cannotInlineWhenArgIsNotInlinable(s);
	s += noinline_cannotInlineWhenArgIsNotInlinable(acos(s));
	
	return s;
}

void main()
{
	f();
}
