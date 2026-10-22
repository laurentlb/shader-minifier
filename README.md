# Shader Minifier

[![Build status](https://ci.appveyor.com/api/projects/status/9sekrxuiway58su3?svg=true)](https://ci.appveyor.com/project/laurentlb/shader-minifier-r5gmt)

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

Shader Minifier is available:
* As an online website: https://ctrl-alt-test.fr/minifier/
* As a command-line tool (download from [Releases](https://github.com/laurentlb/shader-minifier/releases))
* As a .NET library (see [Example](https://github.com/laurentlb/shader-minifier/tree/master/Example))

To be notified of new releases, use the watch feature of GitHub.

## Features

- Parsing and printing of GLSL or HLSL code.
- Generation of a file (such as a C header) that can be embedded in an application.
- A command-line interface that can fit in a build.
- [A web interface](https://ctrl-alt-test.fr/minifier/), for interactive workflows.
- Ability to minify multiple shaders at once, in a consistent way.

See the list of [transformations](TRANSFORMATIONS.md) for more information.
In brief:

- Strip spaces, remove comments, remove useless parens.
- Inline functions, variables and constant values.
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
USAGE: shader_minifier [--help] [--version] [-o <string>] [-v] [--debug]
                       [--hlsl]
                       [--format <text|indented|c-variables|c-array|js|nasm|rust|json>]
                       [--field-names <rgba|xyzw|stpq>] [--preserve-externals]
                       [--preserve-all-globals] [--no-inlining]
                       [--aggressive-inlining] [--no-renaming]
                       [--no-renaming-list <string>] [--no-sequence]
                       [--no-remove-unused] [--no-overloading] [--move-declarations]
                       [--preprocess] [-D <string>]
                       [--preprocess-no-expand <string>]
                       [--export-kkp-symbol-maps] [--readable-output <string>]
                       [<filename>...]

FILENAMES:

    <filename>...         List of files to minify (use - for stdin)

OPTIONS:

    --version             Display the version and exit
    -o, --output <string> Set the output filename (default is shader_code.h,
                          use - for stdout)
    -v, --verbose         Verbose, display additional information
    --debug               Debug, display more additional information
    --hlsl                Use HLSL (default is GLSL)
    --format <text|indented|c-variables|c-array|js|nasm|rust|json>
                          Choose to format the output (use 'text' if you want
                          just the shader)
    --field-names <rgba|xyzw|stpq>
                          Choose the field names for vectors: 'rgba', 'xyzw',
                          or 'stpq'
    --preserve-externals  Do not rename external values (e.g. uniform)
    --preserve-all-globals
                          Do not rename functions and global variables
    --no-inlining         Do not automatically inline variables and functions
    --aggressive-inlining Aggressively inline constants. This can reduce output
                          size due to better constant folding. It can also
                          increase output size due to repeated inlined
                          constants, but this increased redundancy can be
                          beneficial to compression, leading to a smaller final
                          compressed size anyway. Does nothing if inlining is
                          disabled.
    --no-renaming         Do not rename anything
    --no-renaming-list <string>
                          Comma-separated list of functions to preserve, in
                          addition to main and mainImage
    --no-sequence         Do not use the comma operator trick
    --no-remove-unused    Do not remove unused code
    --no-overloading      When renaming functions, do not introduce new
                          overloads
    --move-declarations   Move declarations to group them
    --preprocess          Evaluate the preprocessor directives and expand the
                          macros
    -D <string>           With --preprocess, define a macro (e.g. -D DEBUG=0 or
                          -D FAST)
    --preprocess-no-expand <string>
                          With --preprocess, comma-separated list of macros to
                          keep (not expand)
    --export-kkp-symbol-maps
                          Export kkpView symbol maps (written next to each
                          input file, with the .sym extension)
    --readable-output <string>
                          Also write the code before renaming, in indented
                          format, to this file (same as --no-renaming --format
                          indented)
    --help                display this list of options.
```

In short:

* List the shaders you want to minify on the command-line. Use `-` to read a
  shader from stdin.

* Use `-o` to choose the output file (by default, it will use `shader_code.h`).
  If you pass `-` for the output, it will be printed on stdout.

* Use `--format` to control the output format. By default, it will create a C
  header. There are other options to get only the shader, or have it in a
  Javascript, Rust, or nasm file.

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
#define SHADER_MINIFIER_IMPL
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

To get both outputs in a single run, use `--readable-output`. It writes the
same code as `--format indented --no-renaming` to a second file:

```
shader_minifier.exe -o shader_code.h --readable-output shader.min.frag fragment.glsl
```

Committing that file to your repository lets you see, in each diff, how a
source change affects the minified code (e.g. a new function call that pulls in
many dependencies).

## Concepts

### Shader behaviour

Shader Minifer works by applying to the
[AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) modifications
that produce a transformed but semantically equivalent AST. The
resulting assembly may be different, but the minified shader should
have the same behaviour as the original one. Or at least that's the
intent.

Some of the [transformations](TRANSFORMATIONS.md) are not completely safe and may break in some corner
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

### Evaluation of preprocessor directives

With the flag `--preprocess`, Shader Minifier runs a partial preprocessor
before minifying the code:

* Macros (`#define`) are expanded, including function-like macros, and their
    `#define` line is removed. `#undef` is supported.
* Conditions (`#if`, `#ifdef`, `#ifndef`, `#elif`, `#else`) are evaluated and
    the dead code is removed. Expressions support `defined`, integer
    arithmetic, comparisons and logical operators. As in C, an undefined macro
    is false.
* Use `-D NAME=VALUE` (or `-D NAME`, which means `NAME=1`) to define a macro
    from the command-line.
* Use `--preprocess-no-expand A,B` to keep some macros in the output (e.g.
    macros that save bytes). Their value is still used to evaluate the
    conditions.

For example, this removes the debug code:

```c
#define DEBUG 0
...
#if DEBUG
...
#endif
```

The preprocessing is partial: macros starting with `GL_` or `__` (e.g. `GL_ES`,
`__VERSION__`) are defined by the implementation, so their value is unknown.
When a condition cannot be evaluated, the directive is kept in the output, and
both branches are preprocessed. Macros that are defined or undefined in such a
region are not expanded, and all their `#define` lines are kept.

Limitations: the operators `#` and `##` are not supported, as well as variadic
macros.

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

When renaming functions, Shader Minifier will try to introduce as much
overloading as possible. This reduces the number of identifiers used by the
shader and makes it more compression-friendly.

Shader Minifier works best when the input code doesn't use function overloading
(two functions with the same name and different arguments). Function overloading
might confuse the analysis and lead to an output that doesn't compile. Use the
flag `--no-overloading` if you don't want Shader Minifier to introduce function
overloading, it's probably safer. Please also file issues related to function
overloading, as we don't have enough test coverage at the moment.


### kkpView symbol maps

Shader Minifier can export symbol files that map the minified code back to names
from the original source code. This lets you visualize shader size using
https://github.com/ConspiracyHu/rekkrunchy-with-analytics

### Shader Minifier performance

On my machine, it takes around 1s to minify a shader. A lot of that time comes
from the startup time of the binary. If you have many shaders, try to minify
them all at the same time by listing them all on the command-line.


## Transformations

- [Whitespace and comments removal](TRANSFORMATIONS.md#Whitespace-and-comments-removal)
- [Parentheses simplifications](TRANSFORMATIONS.md#Parentheses-simplifications)
- [Curly braces removal](TRANSFORMATIONS.md#Curly-braces-removal)
- [Literal numbers](TRANSFORMATIONS.md#Literal-numbers)
- [Constant arithmetic](TRANSFORMATIONS.md#Constant-arithmetic)
- [Conditionals evaluation](TRANSFORMATIONS.md#Conditionals-evaluation)
- [Commutative operators](TRANSFORMATIONS.md#Commutative-operators)
- [Comma operator](TRANSFORMATIONS.md#Comma-operator)
- [Useless else after return](TRANSFORMATIONS.md#Useless-else-after-return)
- [Ternary operator](TRANSFORMATIONS.md#Ternary-operator)
- [Loops](TRANSFORMATIONS.md#Loops)
- [Merge declarations](TRANSFORMATIONS.md#Merge-declarations)
- [Group declarations](TRANSFORMATIONS.md#Group-declarations)
- [Augmented operators](TRANSFORMATIONS.md#Augmented-operators)
- [Rename vector fields](TRANSFORMATIONS.md#Rename-vector-fields)
- [Automatic inlining](TRANSFORMATIONS.md#Automatic-inlining)
- [Aggressive inlining](TRANSFORMATIONS.md#Aggressive-inlining)
- [Explicit inlining](TRANSFORMATIONS.md#Explicit-inlining)
- [Reassignment merging](TRANSFORMATIONS.md#Reassignment-merging)
- [Variable reuse](TRANSFORMATIONS.md#Variable-reuse)
- [Vector constructors](TRANSFORMATIONS.md#Vector-constructors)
- [Function predeclarations](TRANSFORMATIONS.md#Function-predeclarations)
- [Unused local variables](TRANSFORMATIONS.md#Unused-local-variables)
- [Dead code removal](TRANSFORMATIONS.md#Dead-code-removal)
- [Unused function removal](TRANSFORMATIONS.md#Unused-function-removal)
- [Other functions](TRANSFORMATIONS.md#Other-functions)
- [Renaming](TRANSFORMATIONS.md#Renaming)


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
  