Shader Minifier
===============

Shader Minifier is a tool that minifies and obfuscates shader code (GLSL
and HLSL). It's mainly target at demosceners, for writing 4k and 64k
intros.


This is the initial source release. The code compiles
with F# 2.0 and Visual Studio 2010, and has not been tested with other
versions.

The code is a bit messy, and mostly undocumented. Sorry about that. I
wanted to release it anyway by popular demand. I'll try to clean it, but
feel free to contact me if you have any question.


Slightly outdated user manual:
  http://www.ctrl-alt-test.fr/?page_id=7


Some technical details
----------------------

If I remember correctly, I patched FParsec.dll as it didn't work properly
on Mono because of a bug in Mono (https://bugzilla.novell.com/show_bug.cgi?id=474154).

Because of a bug in F#, I'm targeting .NET 3.5 instead of .NET 4.0. The
bug happens when using at the same time F# powerpack and the --standalone flag.


Those bugs make the release process is a bit messy. I haven't checked
recently, so some of the bugs might have been fixed. For reference, I'll
give the exact command-lines I use (paths have to be updated).

For Windows, I compile with --standalone, targeting .NET 3.5:

"c:\Program Files (x86)\Microsoft F#\v4.0\fsc.exe" -o shader_minifier.exe --debug:pdbonly --noframework --define:TRACE --optimize+ -r:D:\Laurent\cat\glsl_minifier\FParsec.dll -r:D:\Laurent\cat\glsl_minifier\FParsecCS.dll -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\2.0\Runtime\v2.0\FSharp.Core.dll" -r:D:\Laurent\cat\glsl_minifier\bin\Release\FSharp.PowerPack.dll -r:C:\Windows\Microsoft.NET\Framework\v2.0.50727\mscorlib.dll -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\v3.5\System.Core.dll" -r:C:\Windows\Microsoft.NET\Framework\v2.0.50727\System.dll --target:exe --warn:3 --warnaserror:76 --LCID:1033 --utf8output --fullpaths --flaterrors --standalone ast.fs printer.fs cGen.fs renamer.fs rewriter.fs parse.fs main.fs

For Linux, I use a newer version of FParsec that I patched manually (some
functions in FParsec have changed, which means I need a different parse.fs
source file too). This newer FParsec version requires .NET 4.0, which is
why I cannot use --standalone and have to ship the dll files separately.

rem "c:\Program Files (x86)\Microsoft F#\v4.0\fsc.exe" -o shader_minifier.exe --debug:pdbonly --noframework --define:TRACE --optimize+ -r:D:\Laurent\cat\glsl_minifier\FParsec.new.dll -r:D:\Laurent\cat\glsl_minifier\FParsecCS.new.dll -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\2.0\Runtime\v4.0\FSharp.Core.dll" -r:D:\Laurent\cat\glsl_minifier\bin\Release\FSharp.PowerPack.dll -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\mscorlib.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\System.Core.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\System.dll" --target:exe --warn:3 --warnaserror:76 --vserrors --LCID:1033 --utf8output --fullpaths --flaterrors "C:\Users\laurent\AppData\Local\Temp\.NETFramework,Version=v4.0.AssemblyAttributes.fs" ast.fs printer.fs cGen.fs renamer.fs rewriter.fs parse.new.fs main.fs


It would be nice to have a simpler way to release stuff.




Laurent Le Brun, aka LLB from Ctrl-Alt-Test.

  http://laurent.le-brun.eu
  http://ctrl-alt-test.fr
