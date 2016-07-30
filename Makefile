HASKELL_SRCS = $(wildcard *.hs)
LIBSRCS = $(wildcard lib/*.s)
LIBS = $(patsubst %.s,%.o,$(LIBSRCS))
INCLUDEDIR=include/
CPP=gcc -nostdinc -I $(INCLUDEDIR) -E
run: out
	@echo "Emulator output:"
	@python3 ~/programming/d16i/run_d16i.py -q out
	
%.o: %.s
	d16 $< -o $@

out: start.o $(LIBS) compiler.o
	d16-ld $^ -o out
	
compiler.s: cmm test.c
	$(CPP) test.c -o test.i
	./cmm test.i compiler.s
	
cmm: $(HASKELL_SRCS)
	ghc --make Main.hs -o cmm

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
	-$(MAKE) -C test
