# Shader Minifier

[![Build status](https://ci.appveyor.com/api/projects/status/chwlpnssgd5kdl4x/branch/master?svg=true)](https://ci.appveyor.com/project/laurentlb/shader-minifier/branch/master)

Shader Minifier is a tool that minifies and obfuscates shader code (GLSL and
HLSL) without affecting its behaviour. It is also suitable for reducing the size
of the shaders in other applications (e.g. webgl, games).

Its original use-case is for the
[demoscene](https://en.wikipedia.org/wiki/Demoscene), for optimizing 4k and 64k
intros. In the context of 4kB intros, Shader Minifier help developers maintain
and iterate on human-readable files, while shipping optimized code. Even when a
shader is minified by hand by experienced demosceners, Shader Minifier is often
able to optimize it further. See this [2010
report](https://www.ctrl-alt-test.fr/2010/glsl-minifier-smaller-and-smaller/).

To be notified of new releases, use the watch feature of GitHub.

Try the online version here: https://ctrl-alt-test.fr/minifier/

## Features

- Parsing and printing of GLSL or HLSL code.
- Generation of a file (such as a C header) that can be embedded in an application.
- A command-line interface that can fit in a build.
- [A web interface](https://ctrl-alt-test.fr/minifier/), for interactive workflows.
- Ability to minify multiple shaders at once, in a consistent way.

See the list of [transformations](#transformations) below for more information.
In brief:

- Strip spaces, remove comments, remove useless parens.
- Inline variables and constant values.
- Simplify constant expressions: `5. * 2.` becomes `10.`.
- Group declarations: `float a=2.;float b;` becomes `float a=2.,b;`.
- Apply other tricks to reduce the file size.
- Simplify calls to vector constructors using vector swizzles.
- Rename variables, typically to one character.
- Remove unused local variables, unused functions and other dead code.

Other transformations try to make the code more compression friendly, e.g.

- Consistently rename vector fields (e.g. use `foo.xy` instead of `foo.rg`) to
help the compression.
- Reuse the variable names as much as possible: a local variable may have the
same name as a global variable not used in the function; two functions may have
the same name using function overloading.
- Analyze the context and make statistics to compute the variable name that will
be the most compression-friendly.


## Example output

```c
/* File generated with Shader Minifier 1.3
 * http://www.ctrl-alt-test.fr
 */
#ifndef HEART_FRAG_EXPECTED_
# define HEART_FRAG_EXPECTED_
# define VAR_mouse "f"
# define VAR_resolution "y"
# define VAR_time "v"

const char *heart_frag =
 "uniform float v;"
 "uniform vec2 y;"
 "uniform vec4 f;"
 "void main()"
 "{"
   "vec2 f=(2.*gl_FragCoord.xy-y)/y.y;"
   "float r=mod(v,2.)/2.,a=pow(r,.2)*.5+.5;"
   "a-=a*.2*sin(r*6.2831*5.)*exp(-r*6.);"
   "f*=vec2(.5,1.5)+a*vec2(.5,-.5);"
   "float m=atan(f.x,f.y)/3.141593,x=length(f),e=abs(m),o=(13.*e-22.*e*e+10.*e*e*e)/(6.-5.*e),n=step(x,o)*pow(1.-x/o,.25);"
   "gl_FragColor=vec4(n,0.,0.,1.);"
 "}";

#endif // HEART_FRAG_EXPECTED_
```

Multiple output formats are available.

## Usage

Download Shader Minifier here:
  https://github.com/laurentlb/Shader_Minifier/releases

It is a command-line tool. Without argument, it will show the usage. If you are
not on Windows, you will need mono:

```
$ shader_minifier.exe  # Windows
$ mono shader_minifier.exe  # Linux, Mac...
```

```
USAGE: Shader Minifier [--help] [-o <string>] [-v] [--hlsl]
                       [--format <text|indented|c-variables|c-array|js|nasm>]
                       [--field-names <rgba|xyzw|stpq>] [--preserve-externals]
                       [--preserve-all-globals] [--no-inlining]
                       [--aggressive-inlining] [--no-renaming]
                       [--no-renaming-list <string>] [--no-sequence]
                       [--smoothstep] [--no-remove-unused]
                       [--no-move-declarations] [<filename>...]

FILENAMES:

    <filename>...         List of files to minify

OPTIONS:

    -o <string>           Set the output filename (default is shader_code.h)
    -v                    Verbose, display additional information
    --hlsl                Use HLSL (default is GLSL)
    --format <text|indented|c-variables|c-array|js|nasm>
                          Choose to format the output (use 'text' if you want
                          just the shader)
    --field-names <rgba|xyzw|stpq>
                          Choose the field names for vectors: 'rgba', 'xyzw',
                          or 'stpq'
    --preserve-externals  Do not rename external values (e.g. uniform)
    --preserve-all-globals
                          Do not rename functions and global variables
    --no-inlining         Do not automatically inline variables
    --aggressive-inlining Aggressively inline constants. This can reduce output
                          size due to better constant folding. It can also
                          increase output size due to repeated inlined
                          constants, but this increased redundancy can be
                          beneficial to gzip leading to a smaller final
                          compressed size anyway. Does nothing if inlining is
                          disabled.
    --no-renaming         Do not rename anything
    --no-renaming-list <string>
                          Comma-separated list of functions to preserve
    --no-sequence         Do not use the comma operator trick
    --smoothstep          Use IQ's smoothstep trick
    --no-remove-unused    Do not remove unused code
    --no-move-declarations
                          Do not move declarations to group them
    --help                display this list of options.
```

In short:

* List the shaders you want to minify on the command-line.

* Use `-o` to choose the output file (by default, it will use `shader_code.h`).
  If you pass `-` for the output, it will be printed on stdout.

* Use `--format` to control the output format. By default, it will create a C
  header. There are other options to get only the shader, or have it in a .js or
  nasm file.

## Tips

### 4kB intros

4kB intros typically use a single shader file. The default flags should work well:

```
shader_minifier.exe -o shader_code.h fragment.glsl
```

We recommend that you frequently check the output of Shader Minifier to make
sure there are no obvious problems. Use [inlining](#inlining) where possible.

If you desperately need to save a few bytes, try another value of
`--field-names`. It can affect the size after compression.

### 64kB intros

The recommandation is to use:

```
shader_minifier.exe --format c-array *.frag -o shaders.h
```

Then, in your C or C++ code, include the file:

```c
const char* shaderSources[] = {
#include shaders.h
};
```

Note that uniforms will be renamed consistently across all the files. The
`#define` lines will tell you how they were renamed. To disable this renaming,
use `--preserve-externals`.

### Javascript

Use `--format js`. It will define a variable for each shader, the variable name
being derived from the input file. We expect you to run a Javascript minifier on
the output file.

### Other applications

The simplest solution is to minify each file separately:

```
shader_minifier.exe --format text --preserve-externals file.glsl -o file_opt.glsl
```

The output may be used as a drop-in replacement for your original shaders.

### Debugging

To better understand what Shader Minifier changed and get a more readable
output, use the flags `--format indented --no-renaming`. This will add some
indentation to the output, instead of using overly long lines.

## Concepts

### Shader behaviour

Shader Minifer works by applying to the
[AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) modifications
that produce a transformed but semantically equivalent AST. The
resulting assembly may be different, but the minified shader should
have the same behaviour as the original one. Or at least that's the
intent.

Some of the transformations are not completely safe and may break in some corner
cases. If you observe differences, don't hesitate to [report a bug](#Feedback).

### Macros

Shader Minifier will preserve the preprocessor directives (the lines starting
with `#`), except that it strips the spaces.

If you define a macro, Shader Minifier will notice the name of the macro and
won't rename the occurrences of the macro. It also doesn't rename variables used
inside the macro. Clever macros are discouraged and can break the shader.

Avoid macros that contain references to other variables, or affect how the
code should be parsed. You may get a parse error in Shader Minifier, or get an
output that won't compile.

### Verbatim

If you want to temporary turn off the minifier, use the `//[` and `//]`
comments. This can be useful as a workaround if you get a parser error.

Variables inside the region won't be renamed. Spaces will be stripped.

```glsl
//[
layout(local_size_x = 32) in;
//]
```

### Overloading

At this time, do not use overloaded functions (two functions with the same name
but different arguments) in the input. The output probably won't compile.

On the other hand, Shader Minifier will aggressively use function overloading in
the output. If two functions have a different number of arguments, they may have
the same name in the output. This reduces the number of identifiers used by the
shader and make it more compression friendly.

### Shader Minifier performance

On my machine, it takes around 1s to minify a shader. A lot of that time comes
from the startup time of the binary. If you have many shaders, try to minify
them all at the same time by listing them all on the command-line.

## Transformations

### Whitespace and comments removal

All comments are removed. Whitespace is removed whenever possible.

This transformation cannot be disabled, although you may use `--format indented`
to see an indented output.

### Parentheses simplifications

Parentheses that are not needed are automatically removed. This transformations cannot be disabled; parentheses are not even recorded in the AST, they are computed based on operator precedence rules.

Input:
```glsl
int a = (x * x) * (x + 1);
```

Output:
```glsl
int a = x * x * (x + 1);
```

### Curly braces removal

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

### Literal numbers

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

### Constant arithmetic

Operations with constant arguments are evaluated, e.g. `5*2` will be replaced
with `10`. This is useful especially when a value has been inlined.

### Conditionals evaluation

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


### Commutative operators

If you write `x*(y*z)`, Shader Minifier will not remove the parentheses because
it would change the orders of evaluation (this could be a problem with floating
point numbers and affect the precision of the result). Instead, it will swap the
operands and return `y*z*x`.

We apply this technique in a few cases:
- `x*(y*z)` becomes `y*z*x`
- `x+(y+z)` becomes `y+z+x`
- `x+(y-z)` becomes `y-z+x`
- `x-(y+z)` becomes `x-y-z`
- `x-(y-z)` becomes `x-y+z`

### Comma operator

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

### Useless else after return

When a if always ends in a return, the else can be omitted entirely.

Input:
```
if(c)return a();else b();
```

Output:
```
if(c)return a();b();
```

### Ternary operator

When both branches of a if+else are only expressions and they end with
an assignment to the same variable, the if is changed into a ternary operator.

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

### Merge declarations

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

### Group declarations

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
the shader slightly longer. When the variables are renamed, this should rarely
be an issue.

Disable this transformation with the flag `--no-move-declarations`.

### Augmented operators

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

### Rename vector fields

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

### Automatic inlining

Shader Minifier will try to automatically inline some variables.
This happens when:
- the variable is used only once in the current block,
- and the variable is not used in a sub-block (e.g. inside a loop),
- and the init value is trivial (doesn't depend on a variable).

If inlining causes a bug in your code, you can disable it with `--no-inlining`
and please report a bug.

### Aggressive inlining

Shader Minifier can optionally/experimentally inline even more aggressively.
Along with the above cases, it will inline *any* variable marked `const`, and
also when:
- the variable is never written to after initalization
- and the init value is trivial (doesn't depend on a variable).

This is enabled with `--aggressive-inlining`.

**Note**: Inlining can lead to repetition in the shader code, which may make the
shader longer, but the output may be more compression-friendly.

### Explicit inlining

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

```c
int foo(int x)
{
  return 10*x;
}
```

And this input:

```c
float i_foo(float f, float g, float x) {
  return f*x + g*x + f*g;
}

float bar(float a) {
  return i_foo(2.0, 3.0, sin(sqrt(a)));
}
```

will be simplified into:

```c
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

### Vector constructors

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

### Function predeclarations

Function predeclarations are removed by the parser. Then Shader Minifier
reorders the functions in the file, so that functions are declared before being
called.

**Note**: Function reordering may break if there are `#if` statements at the
top-level.

### Unused local variables

Local variables that are not referenced are removed.

**Note**: In theory, this can change the behavior of the code, if the
initialization value calls a function that performs a side-effect.

### Dead code removal

The code after a `return` is removed.

**Note**: This is optimization is disabled when there are preprocessor
directives (e.g. `#if`) in the block.

### Unused function removal

Functions that are not called are removed. Functions listed with the flag
`--no-renaming-list` are considered as entry-point and are not removed. By
default, this is the case for `main` and `mainImage`.

**Note**: Use `--no-remove-unused` to disable this transformation.


### Smoothstep transformation

`smoothstep(a,b,x)` calls can be replaced with
`smoothstep(0.0,1.0,(x-a)/(b-a))`. When `a` and `b` are constant, the expression
will be simplified. In some cases, this might make the code more compressible,
and this technique was [used in
Elevated](https://www.pouet.net/topic.php?which=6751&page=1#c295695).

However, in many cases this trick doesn't give good results. As a result, this
is not enabled by default; use `--smoothstep` if you want to try it.

### Renaming

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

## Feedback

Please give feedback in the [bugtracker](https://github.com/laurentlb/Shader_Minifier/issues).
If something is blocking you, you can file a bug or update an existing bug. We
rely on your feedback to prioritize the work.

Feature requests are encouraged. Add upvotes or comments to existing feature
requests that are important to you.

Contributions are welcome. Please leave a message before implementing a
significant change, so that we can agree on the solution.

---

Created by Laurent Le Brun (LLB / Ctrl-Alt-Test) and
[other contributors](https://github.com/laurentlb/Shader_Minifier/graphs/contributors).

  http://laurent.le-brun.eu &mdash; http://ctrl-alt-test.fr
  