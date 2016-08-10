LIBSRCS = $(wildcard ../lib/*.d16)
LIBS = $(patsubst %.d16,%.o,$(LIBSRCS))
TESTS = $(wildcard *.c)
TEST_BINS = $(patsubst %.c,%.bin,$(TESTS))
GREEN_COLOR='\033[0;32m'
NO_COLOR='\033[0m'
CC=../cmm
CPP=gcc -nostdinc -I../include -E
test: $(TEST_BINS)
	@echo "$(GREEN_COLOR)All tests successful$(NO_COLOR)"
	@rm *.bin
	
%.o:%.c 
%.i: %.c
	$(CPP) $< -o $@

%.s: %.i
	@../cmm $< $@ >/dev/null
%.o: %.s
	@d16 $< -o $@
%.o: %.d16
	@d16 $< -o $@
%.bin: ../start.o %.o $(LIBS)
	@d16-ld $^ -o $@
	@python run_test.py $@ $*.output
	@echo "$(GREEN_COLOR)Test of $(*).cm successful$(NO_COLOR)"
