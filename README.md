# Haskell C- Compiler
This is an in-progress compiler for the [C- language](http://www.cs.dartmouth.edu/~cs57/Project/C-%20Spec.pdf).
To compile a C source file for the d16 processor:
```
$(CC) -E -nostdinc -Iinclude/ file.c -o file.i
./cmm file.i file.s
d16 file.s -o file.o
d16-ld start.o lib/*.o file.o -o file
```
and optionally to run the file in the d16i emulator:
```
d16i -q file
```

## Dependencies:
Ghc

cmake

gcc/clang

[d16 assembler](https://www.github.com/C-Elegans/d16)

[d16 linker](https://www.github.com/C-Elegans/d16-ld)

[d16 emulator](https://www.github.com/flaviut/d16i)


To compile the compiler, run `cabal configure` and `cabal build`.
