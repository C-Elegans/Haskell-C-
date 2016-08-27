HASKELL_SRCS = $(wildcard src/*.hs)
LIBSRCS = $(wildcard lib/*.d16)
LIBS = $(patsubst %.d16,%.o,$(LIBSRCS))
INCLUDEDIR=include/
CPP=gcc -nostdinc -I $(INCLUDEDIR) -E
run: out
	@echo "Emulator output:"
	@d16i -q out

%.o: %.s
	d16 $< -o $@
%.o: %.d16
	d16 $< -o $@
out: start.o $(LIBS) compiler.o
	d16-ld $^ -o out

%.i: %.c
	$(CPP) test.c -o test.i
compiler.s: cmm test.i
	./cmm test.i compiler.s

cmm: $(HASKELL_SRCS)
	cabal build
	-rm -f cmm
	ln -s dist/build/cmm/cmm cmm

clean-all: clean
	-rm -f cmm
	-rm -f *.o
	-rm -f *.hi

clean:
	-rm -f out.o
	-rm -f lib/*.o
	-rm -f compiler.s
	-rm -f out.s

check: cmm
	export INCLUDEDIR
	export CPP
	$(MAKE) -C test
