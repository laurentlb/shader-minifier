 #! /bin/bash

references='-r lib/FParsec.dll -r lib/FParsecCS.dll -r lib/FSharp.PowerPack.dll'
fsharpc --standalone $references src/{ast.fs,printer.fs,cGen.fs,renamer.fs,rewriter.fs,parse.fs,main.fs} -o shader_minifier.exe
