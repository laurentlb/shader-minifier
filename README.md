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



Laurent Le Brun, aka LLB from Ctrl-Alt-Test.
