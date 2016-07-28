HASKELL_SRCS = $(wildcard *.hs)
LIBSRCS = $(wildcard lib/*.s)
LIBS = $(patsubst %.s,%.o,$(LIBSRCS))
run: out
	@echo "Emulator output:"
	@python3 ~/programming/d16i/run_d16i.py -q out
	
%.o: %.s
	d16 $< -o $@

out: start.o $(LIBS) compiler.o
	d16-ld $^ -o out
	
compiler.s: cmm test.cm
	./cmm test.cm compiler.s
	
cmm: $(HASKELL_SRCS)
	ghc --make Main.hs -o cmm

clean:
	-rm -f out.o
	-rm -f cmm
	-rm -f compiler.s
	-rm -f out.s
