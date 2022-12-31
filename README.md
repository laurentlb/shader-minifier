# Shader Minifier

[![Build status](https://ci.appveyor.com/api/projects/status/chwlpnssgd5kdl4x/branch/master?svg=true)](https://ci.appveyor.com/project/laurentlb/shader-minifier/branch/master)

Shader Minifier is a tool that minifies and obfuscates shader code
(GLSL and HLSL) without affecting its behaviour. Its original use-case
is for the [demoscene](https://en.wikipedia.org/wiki/Demoscene), for
optimizing 4k and 64k intros. It is also suitable for reducing the size
of the shaders in other applications (e.g. webgl, games).

In the context of 4kB intros, Shader Minifier help developers maintain and
iterate on human-readable files, while shipping optimized code. Even when a
shader is minified by hand by experienced demosceners, Shader Minifier is often
able to optimize it further. See this
[2010 report](https://www.ctrl-alt-test.fr/2010/glsl-minifier-smaller-and-smaller/).

To be notified of new releases, use the watch feature of GitHub.

Try the online version here: https://ctrl-alt-test.fr/minifier/

## Features

- Parse and print the GLSL or HLSL code.
- Generate a file (such as a C header) that can be embedded in an application.
- Strip spaces, remove comments.
- Remove useless parens.
- Simplify constant expressions: `3.14159 * 2.` becomes `6.28318`.
- Remove curly braces whenever possible: `if(test){v.x=4; return b++;}` is
replaced with `if(test)return v.x=4,b++;`.
- Squeeze definitions: `float a=2.;float b;` becomes `float a=2.,b;`.
- Consistently rename vector fields (e.g. use `foo.xy` instead of `foo.rg`) to
help the compression.
- Rename variables, typically to one character.
- Reuse the variable names as much as possible: a local variable may have the
same name as a global variable not used in the function; two functions may have
the same name using function overloading.
- Analyze the context and make statistics to compute the variable name that will
be the most compression-friendly.
- Inline variables.
- Remove unused local variables.

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
USAGE: shader_minifier [--help] [-o <string>] [-v] [--hlsl]
                       [--format <text|indented|c-variables|c-array|js|nasm>]
                       [--field-names <rgba|xyzw|stpq>] [--preserve-externals]
                       [--preserve-all-globals] [--no-inlining]
                       [--aggressive-inlining] [--no-renaming]
                       [--no-renaming-list <string>] [--no-sequence]
                       [--smoothstep] [<filename>...]

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

## Macros

Shader Minifier will preserve the preprocessor directives (the lines starting
with `#`), except that it strips the spaces.

If you define a macro, Shader Minifier will notice the name of the macro and
won't rename the occurrences of the macro. It also doesn't rename variables used
inside the macro. Clever macros are discouraged and can break the shader.

## Verbatim

If you want to temporary turn off the minifier, use the `//[` and `//]` comments. This can be useful as a workaround if you get a parser error.

Variables inside the region won't be renamed. Spaces will be stripped.

```c
//[
[maxvertexcount(3)]
//]
void GSScene( triangleadj GSSceneIn input[6], inout TriangleStream<PSSceneIn> OutputStream )
{   
    PSSceneIn output = (PSSceneIn)0;
    ...
}
```

## Overloading

At this time, do not use overloaded functions (two functions with the same name
but different arguments) in the input. The output probably won't compile.

On the other hand, Shader Minifier will aggressively use function overloading in
the output. If two functions have a different number of arguments, they may have
the same name in the output. This reduces the number of identifiers used by the
shader and make it more compression friendly.

## Shader behaviour

Shader Minifer works by applying to the
[AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) modifications
that produce a transformed but semantically equivalent AST. The
resulting assembly may be different, but the minified shader should
have the same behaviour as the original one. Or at least that's the
intent.

However certain rules, especially floating point arithmetic, can be
tricky. If you observe differences, don't hesitate to
[report a bug](#Feedback).

## Bugs and limitations

- The parser is not complete. Some constructs are not yet supported.
- If possible, avoid overloaded functions; some transformations might not handle
  them properly.
- Avoid macros that contain references to other variables, or affect how the
  code should be parsed. You may get a parse error in Shader Minifier, or get an
  output that won't compile.

Please [give feedback](#Feedback) if these limitations affect you.

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

### Constant arithmetic

Operations with constant arguments are evaluated, e.g. `5*2` will be replaced
with `10`. This is useful especially when a value has been inlined.

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

### Definition merging

If multiple values of the same type are defined next to each other, we can merge
the definitions.

Input:
```glsl
float a = 1.;
float b = 2.;
```

Output:
```glsl
float a = 1., b = 2.;
```

**Note**: The order of the definitions is preserved. Try to group definitions by
type before running Shader Minifier.

```glsl
int x;
float y;
int z;    // Not optimized! Move this line above.
```

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

```c
bool i_debug = false;
int i_level = 5;

int foo(int x, int y) {
  if (i_debug) {
    x++;
  }

  return 2 * i_level * x;
}
```

will be simplified into:

```c
int foo(int x,int y)
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

Calls to `vec2`, `vec3`, and `vec4` can be simplified using swizzles.

Input:
```glsl
vec4(v1.x, v1.z, v2.r, v2.t)
```

Output:
```glsl
vec4(v1.xz,v2.xy)
```

<!--
To document:
- renaming
- unused local variables
- function headers
- function reordering
- unused functions removal
- smoothstep trick
- rewrite numbers / PI detection
-->

## Feedback

Please give feedback in the [bugtracker](https://github.com/laurentlb/Shader_Minifier/issues).
If something is blocking you, you can file a bug or update an existing bug. We
rely on your feedback to prioritize the work.

Contributions are welcome.

---

Created by Laurent Le Brun (LLB / Ctrl-Alt-Test) and
[other contributors](https://github.com/laurentlb/Shader_Minifier/graphs/contributors).

  http://laurent.le-brun.eu -- http://ctrl-alt-test.fr
