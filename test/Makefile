LIBSRCS = $(wildcard ../lib/*.s)
LIBS = $(patsubst %.s,%.o,$(LIBSRCS))
TESTS = $(wildcard *.cm)
TEST_BINS = $(patsubst %.cm,%.bin,$(TESTS))
GREEN_COLOR='\033[0;32m'
NO_COLOR='\033[0m'
CC=../cmm
test: $(TEST_BINS)
	@echo "$(GREEN_COLOR)All tests successful$(NO_COLOR)"
	@rm *.bin
	

%.s: %.cm
	@$(CC) $< $@ >/dev/null
%.o: %.s
	@d16 $< -o $@
%.bin: ../start.o %.o $(LIBS)
	@d16-ld $^ -o $@
	@python run_test.py $@ $*.output
	@echo "$(GREEN_COLOR)Test of $(*).cm successful$(NO_COLOR)"