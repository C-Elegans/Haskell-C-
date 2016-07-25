HASKELL_SRCS = $(wildcard *.hs)

out.o: out.s
	d16 out.s out.o
	python ~/programming/d16i/run_d16i.py out.o

out.s: compiler.s start.s lib/numeric.s
	cat start.s lib/numeric.s compiler.s > out.s

compiler.s: cmm test.cm
	./cmm test.cm compiler.s
	
cmm: $(HASKELL_SRCS)
	ghc --make Main.hs -o cmm

clean:
	-rm -f out.o
	-rm -f cmm
	-rm -f compiler.s
	-rm -f out.s
