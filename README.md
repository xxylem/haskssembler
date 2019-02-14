# Haskssembler

Given an input file.asm in Hack assembly language, 
will parse file and convert to Hack machine language.
Output file is written to same directory with
same filename and extension .hack

Example usage:
haskssembler.exe file.asm

Note: Ver 0. No support for Labels or Symbols
A Instructions are only valid with integers,
e.g. @3, @500
