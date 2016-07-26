HASKELL_SRCS = $(wildcard *.hs)
LIBS = $(wildcard lib/*.s)

run: out.o 
	@echo "Emulator output:"
	@python3 ~/programming/d16i/run_d16i.py -q out.o
	
out.o: out.s
	d16 out.s out.o

out.s: compiler.s start.s $(LIBS)
	cat start.s $(LIBS) compiler.s > out.s

compiler.s: cmm test.cm
	./cmm test.cm compiler.s
	
cmm: $(HASKELL_SRCS)
	ghc --make Main.hs -o cmm

clean:
	-rm -f out.o
	-rm -f cmm
	-rm -f compiler.s
	-rm -f out.s
