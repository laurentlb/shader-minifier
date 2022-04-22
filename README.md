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

If your application uses multiple shaders, use the `--preserve-externals`
option. Values such as the uniforms won't be renamed, which makes it easier to
use in your application (at the expense of a slightly bigger shader).

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
/* File generated with Shader Minifier 1.1.6
 * http://www.ctrl-alt-test.fr
 */
#ifndef HEART_FRAG_EXPECTED_
# define HEART_FRAG_EXPECTED_
# define VAR_MOUSE "f"
# define VAR_RESOLUTION "y"
# define VAR_TIME "v"

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
   "float m=atan(f.x,f.y)/3.14159,x=length(f),e=abs(m),o=(13.*e-22.*e*e+10.*e*e*e)/(6.-5.*e),l=step(x,o)*pow(1.-x/o,.25);"
   "gl_FragColor=vec4(l,0.,0.,1.);"
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
USAGE: shader_minifier.exe [--help] [-o <string>] [-v] [--hlsl] [--format <text|indented|c-variables|c-array|js|nasm>]
                           [--field-names <rgba|xyzw|stpq>] [--preserve-externals] [--preserve-all-globals]
                           [--no-inlining] [--no-renaming] [--no-renaming-list <string>] [--no-sequence] [--smoothstep]
                           [<filename>...]

FILENAMES:

    <filename>...         List of files to minify

OPTIONS:

    -o <string>           Set the output filename (default is shader_code.h)
    -v                    Verbose, display additional information
    --hlsl                Use HLSL (default is GLSL)
    --format <text|indented|c-variables|c-array|js|nasm>
                          Choose to format the output (use none if you want just the shader)
    --field-names <rgba|xyzw|stpq>
                          Choose the field names for vectors: 'rgba', 'xyzw', or 'stpq'
    --preserve-externals  Do not rename external values (e.g. uniform)
    --preserve-all-globals
                          Do not rename functions and global variables
    --no-inlining         Do not automatically inline variables
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
shader_minifier.exe --format c-array --preserve-externals *.frag -o shaders.h
```

Then, in your C or C++ code, include the file:

```c
const char* shaderSources[] = {
#include shaders.h
};
```

Since the uniforms are not renamed, prefer shorter names when
possible. Hopefully a future version of Shader Minifier will improve this.

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

## Inlining

### Automatic inlining

Shader Minifier will try to automatically inline some variables.
This happens when:
- the variable is used only once in the current block,
- and the variable is not used in a sub-block (e.g. inside a loop),
- and the init value is trivial (doesn't depend on a variable).

If inlining causes a bug in your code, you can disable it with `--no-inlining`
and please report a bug.

### Explicit Inlining

Shader Minifier will always inline variables that starts with `i_`. Inlining can allow
the Minifier to simplify the code further.

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

If you want to aggressively reduce the size of your shader, try inlining more
variables. Inlining can have performance implications though (if the variable
stored the result of a computation), so be careful with it.

Inlining can lead to repetition in the shader code, which may make the shader
longer (but the output may be more compression-friendly).

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
tricky. If you observe differences, don't hesitate to report a bug.

## Bugs and limitations

- The parser is not complete. Some constructs are not yet supported.
- Don't use overloaded functions.
- Avoid macros that contain references to other variables.

Please give feedback in the [bugtracker](https://github.com/laurentlb/Shader_Minifier/issues).
If something is blocking you, you can file a bug or update an existing bug. We
rely on your feedback to prioritize the work.

---------

Slightly outdated user manual:
  http://www.ctrl-alt-test.fr/?page_id=7

Contributions are welcome.


Created by Laurent Le Brun (LLB / Ctrl-Alt-Test).

  http://laurent.le-brun.eu
  http://ctrl-alt-test.fr
