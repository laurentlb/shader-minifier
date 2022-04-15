#! /bin/bash

references='-r lib/Argu.dll -r lib/FParsec.dll -r lib/FParsecCS.dll'
fsharpc --standalone $references src/{options.fs,ast.fs,printer.fs,formatter.fs,renamer.fs,rewriter.fs,parse.fs,main.fs} -o shader_minifier.exe
