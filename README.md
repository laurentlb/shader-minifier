# Shader Minifier


Shader Minifier is a tool that minifies and obfuscates shader code
(GLSL and HLSL). Its original use-case is for the
[demoscene](https://en.wikipedia.org/wiki/Demoscene), for optimizing
4k and 64k intros. It is also suitable for reducing the size of the
shaders in other applications (e.g. webgl, games).

The code is tested with:

* F# 4.0 on Windows (Visual Studio Community 2015)
* F# 3.0 on Linux and Mono 3.2.8

Slightly outdated user manual:
  http://www.ctrl-alt-test.fr/?page_id=7

## Usage

Download Shader Minifier here: http://ctrl-alt-test.fr/dl/shader_minifier.exe

It is a command-line tool. Without argument, it will show the usage. If you are
not on Windows, you will need mono:

```
$ shader_minifier.exe  # Windows
$ mono shader_minifier.exe  # Linux, Mac...
```

## Important options

* List the shaders you want to minify on the command-line.

* Use `-o` to choose the output file (by default, it will use `shader_code.h`).
  If you pass an empty string for the output, it will be printed on stdout.

* Use `--format` to control the output format. By default, it will create a C
  header. There are other options to get only the shader, or have it in a .js or
  nasm file.

* Use `--help` to see all the options. You can for example have some control on
  what gets renamed.



Contributions are welcome.


Created by Laurent Le Brun (LLB / Ctrl-Alt-Test).

  http://laurent.le-brun.eu
  http://ctrl-alt-test.fr
