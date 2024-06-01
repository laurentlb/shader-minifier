# Transformations

Here is a list of transformations that Shader Minifier applies to the shader code.

Some of the transformations are not completely safe and may break in some corner
cases. If you observe differences, don't hesitate to [report a bug](#Feedback).


## Whitespace and comments removal

All comments are removed. Whitespace is removed whenever possible.

This transformation cannot be disabled, although you may use `--format indented`
to see an indented output.

## Parentheses simplifications

Parentheses that are not needed are automatically removed. This transformations cannot be disabled; parentheses are not even recorded in the AST, they are computed based on operator precedence rules.

Input:
```glsl
int a = (x * x) * (x + 1);
```

Output:
```glsl
int a = x * x * (x + 1);
```

## Curly braces removal

Curly braces are removed when they are not required. This happens when a block
contains a single statement:

Input:
```glsl
if (cond) {
  return 2;
}
```

Output:
```glsl
if (cond) return 2;
```

**Corner case**: in the example below, the curly braces around the `for` are
needed because of the dangling else problem. If you remove the `else`, braces
will disappear.

```glsl
int dangling_else(int x)
{
  if(x<0)
    {
      for(;;)
        if(x>0)
          return 0;
    }
  else
     return 2;
}
```

## Literal numbers

Numbers are rewritten to take less space without loss of precision.
Additionally, constants that are approximately PI (with 8+ decimal digits) are
are replaced with a call to `acos`.

Input:
```glsl
0.4
0.00000012345
123456700000000
3.14159265
```

Output:
```glsl
.4
1.2345e-7
1.234567e14
acos(-1.)
```

## Constant arithmetic

Operations with constant arguments are evaluated, e.g. `5*2` will be replaced
with `10`. This is useful especially when a value has been inlined.

## Conditionals evaluation

`if` statements, the `||` and `&&` operators, as well as the `? :` ternary
operator are also simplified when a condition is statically known. This is
useful especially after inlining.

Input:
```glsl
const int debug = false;

int foo() {
  int a = 1;
  if (debug) {
    a = 2;
  }
  // ...
}
```

Output:
```glsl
int foo() {
  int a = 1;
  // ...
}
```


## Commutative operators

If you write `x*(y*z)`, Shader Minifier will not remove the parentheses because
it would change the orders of evaluation (this could be a problem with floating
point numbers and affect the precision of the result). Instead, it will swap
the operands and return `y*z*x`. With additive operators, we assume we can
remove the parentheses.

We apply this technique in a few cases:
- `x*(y*z)` becomes `y*z*x`
- `x+(y+z)` becomes `x+y+z`
- `x+(y-z)` becomes `x+y-z`
- `x-(y+z)` becomes `x-y-z`
- `x-(y-z)` becomes `x-y+z`

## Comma operator

When all statements in a block are expression statements (e.g. function calls or
assignments) or return statements, we use the comma operators to reduce the
block to one statement. This allows us to remove the curly braces.

Input:
```glsl
if (x) {
  y = 123;
  x++;
}
```

Output:
```glsl
if (x) y = 123,x++;
```

**Note**: This transformation doesn't always decrease the compressed file size.
Use `--no-sequence` to disable it and see how it performs.

## Useless else after return

When a if always ends in a return, the else can be omitted entirely.

Input:
```
if(c)return a();else b();
```

Output:
```
if(c)return a();b();
```

## Ternary operator

When both branches of a if+else are an expression, the if is changed into a ternary operator.
Additionally, if they end with an assignment to the same variable, the variable is extracted from the ternary operator.

Input:
```glsl
if (c) {
  x = f();
} else {
  a = g();
  x = 1.0;
}
```

Output:
```glsl
x = c ? f() : a = g(), 1.0;
```

When both branches of a if return immediately, the if is changed into a return
that uses a ternary operator.

Input:
```glsl
if (y)
  return z;
return w;
```

Output:
```glsl
return y ? z : w;
```

## Loops

`while` loops are rewritten as `for` loops. This sometimes enable removing semi-colons or braces,
either by moving the preceding statement into the initialization of the `for`,
or by moving the last statement of the loop body into the increment part of the `for`.

Input:
```glsl
i = 0.;
while (i < 50) {
	f(i);
	i++;
}
```

Output:
```glsl
for(i=0.;i<50;i++)f(i);
```

## Merge declarations

If multiple values of the same type are declared next to each other, we can merge
the definitions.

Input:
```glsl
float a = 1.;
float b;
float c = 2.;
```

Output:
```glsl
float a = 1., b, c = 2.;
```

**Note**: The order of the definitions is preserved. Try to group definitions by
type before running Shader Minifier.

```glsl
int x;
float y;
int z;    // Not optimized! Move this line above.
```

The next transformation will take care of this inside function blocks.

## Group declarations

If multiple variables of the same type are declared within the same block, they will be grouped.

Input:
```glsl
int a = 2;
float b = 3;
int c = 4;
```

Input:
```glsl
int a = 2,c;
float b = 3;
c = 4;
```

As shown in the example, the variable name will appear twice (`c` above) while
the type will be written only once (`int` above). In some cases, this might make
the shader slightly longer.

Enable this transformation with the flag `--move-declarations`.

## Augmented operators

We use the augmented operators (e.g. `+=`) where possible.

Input:
```glsl
spe=spe*spe;
x=x-.5;
a=a+(a<<3);
a=a^a>>15;
```

Output:
```glsl
spe*=spe;
x-=.5;
a+=a<<3;
a^=a>>15;
```

This transformation always reduces the size of the output. However, this seems
to have negligible impact on the size after compression.

## Rename vector fields

To access fields of a vector, it is possible to use `.rgba` fields, or `.xyzw`,
or `.stpq`. Shader Minifier will rename them so that it's consistent across all
the shader.

Input:
```glsl
return col.r + col.g + col.b;
```

Outut:
```glsl
return col.x + col.y + col.z;
```

**Note**: Use `--field-names` to select which set of names to use. This can have
a small effect on the compressed file size.

## Automatic inlining

Shader Minifier will try to automatically inline some variables.
This happens when:
- the variable is used only once in the current block,
- and the variable is not used in a sub-block (e.g. inside a loop),
- and the init value is trivial (doesn't depend on a variable).

Shader Minifier will try to automatically inline some functions.
This happens when:
- the function is a single return statement,
- the function is called in only one place,
- the function uses its arguments at most once,
- the function does not modify its arguments,
- the function has no out/inout arguments,
- the function does not use a global that would be shadowed by a local
  that is in scope at the call site.
  
Shader minifier will try to automatically inline an argument of a function call
into the function body, as a declaration that may then be further inlined.
This happens when:
- the argument value is always the same for all call sites.
- the parameter is an 'in' parameter.
- the function is not overloaded (otherwise, removing a parameter could conflict with another overload).

If inlining causes a bug in your code, you can disable it with `--no-inlining`
and please report a bug.

## Aggressive inlining

Shader Minifier can optionally/experimentally inline even more aggressively.
Along with the above cases, it will inline more variables, including the
variables used many times in the code.

This is enabled with `--aggressive-inlining`.

**Note**: Inlining can lead to repetition in the shader code, which may make the
shader longer, but the output may be more compression-friendly.

## Explicit inlining

Shader Minifier will always inline variables and functions that start with
`i_`. Inlining can allow the Minifier to simplify the code further.

For example, this input:

```glsl
bool i_debug = false;
int i_level = 5;

int foo(int x) {
  if (i_debug) {
    x++;
  }

  return 2 * i_level * x;
}
```

will be simplified into:

```glsl
int foo(int x)
{
  return 10*x;
}
```

And this input:

```glsl
float i_foo(float f, float g, float x) {
  return f*x + g*x + f*g;
}

float bar(float a) {
  return i_foo(2.0, 3.0, sin(sqrt(a)));
}
```

will be simplified into:

```glsl
float bar(float a)
{
  return 2.*sin(sqrt(a))+3.*sin(sqrt(a))+6.;
}
```

Note that the function inlining is very simplistic, only supporting functions
which consist of a single return statement. (Basically, cases where you would
use a macro, except this gives the minifier full visibility through it.)

If you want to aggressively reduce the size of your shader, try inlining more
variables. Inlining can have performance implications though (if the variable
stored the result of a computation), so be careful with it.

## Reassignment merging

Shader Minifier can remove or merge assignment statements in some cases:
- if a variable is declared and reassigned on the next line;
- if a variable is reassigned on two consecutive lines;
- if a variable is reassigned and then part of a return.

Of course, this is not always possible (the variable should be read only once;
side-effects might can prevent the optimization).

For example:

```glsl
float d = 0.;
d = min(d, sdfBox(...));
d = min(d, sdfBall(...));
return d;
```

can be simplified into:

```glsl
return min(min(0., sdfBox(...)), sdfBall(...));
```

## Variable reuse

If a local variable is no longer used and we declare a new variable of the
same type, the old variable will be reused instead.

For example:

```glsl
int a = 1;
// ...
f(a);
int b = 2;
// ...
f(b);
```

can be simplified to:

```glsl
int a = 1;
// ...
f(a);
a = 2;
// ...
f(a)'
```

**Note**: This operation reduces the number of variables in the file, which
usually improves the compression. When renaming is disabled, the code can be
misleading, as the variable name won't match the code behavior anymore.

## Vector constructors

- Calls to `vec2`, `vec3`, and `vec4` can be simplified using swizzles.
- Remove useless constructor, when the argument is already a vec using swizzles.
- If all arguments are equal (but not function calls), use only one argument.
- Replace floats with ints, as it is safe inside the vec constructors.

Input:
```glsl
vec4(v1.x, v1.z, v2.r, v2.t);
vec2(v1.xx);
vec2(1.2, 1.2);
vec2(1.);
```

Output:
```glsl
vec4(v1.xz,v2.xy);
v1.xx;
vec2(1.2);
vec2(1);
```

## Function predeclarations

Function predeclarations are removed by the parser. Then Shader Minifier
reorders the functions in the file, so that functions are declared before being
called.

**Note**: Function reordering may break if there are `#if` statements at the
top-level.

## Unused local variables

Local variables that are not referenced are removed.

**Note**: In theory, this can change the behavior of the code, if the
initialization value calls a function that performs a side-effect.

## Dead code removal

The code after a `return` is removed.

**Note**: This is optimization is disabled when there are preprocessor
directives (e.g. `#if`) in the block.

## Unused function removal

Functions that are not called are removed. Functions listed with the flag
`--no-renaming-list` are considered as entry-point and are not removed. By
default, this is the case for `main` and `mainImage`.

**Note**: Use `--no-remove-unused` to disable this transformation.


## Other functions

* `distance(a, b)` becomes `length(a-b)`.
* `pow(x, 1.)` becomes `x`.

## Renaming

There are two renaming strategies:

- If a single shader is passed on the command line, the renaming will reuse
  names aggressively. It will try to find the best name, based on the context
  where the variable is used, in order to make the code more compressible.

- If multiple shaders are minified together, the renaming is consistent across
  all files. For example, all occurrences of a name (even if it's declared
  multiple times in different functions or files) will get renamed the same way.
  So if you have duplicate code in different files, that code will still
  compress well with this renaming strategy.

Renaming variables and functions in a compression-friendly way is difficult.
Future versions of Shader Minifier may use different heuristics.
