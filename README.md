# Haskell C- Compiler
This is an in-progress compiler for the [C- language][].

To compile a C source file for the d16 processor:

```
$(CC) -E -nostdinc -Iinclude/ file.c -o file.i
./cmm file.i file.s
d16 file.s -o file.o
d16-ld start.o lib/*.o file.o -o file
```

and optionally to run the file in the [d16i emulator][]:

```
d16i -q file
```

[C- language]: http://www.cs.dartmouth.edu/~cs57/Project/C-%20Spec.pdf

## Dependencies:

- GHC
- CMake
- gcc/clang
- [d16 assembler][]
- [d16 linker][]
- [d16i emulator][]

[d16 assembler]: https://www.github.com/C-Elegans/d16
[d16 linker]: https://www.github.com/C-Elegans/d16-ld
[d16i emulator]: https://www.github.com/flaviut/d16i

## Build instructions

To compile the compiler, do the following:

```
cabal sandbox init  # set up a local enviroment
cabal install  # install dependnencies
cabal configure
cabal build
./dist/build/cmm/cmm <input> <output>
```
