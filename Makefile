TASKS_HS = $(wildcard T*.hs)
TASKS = $(TASKS_HS:%.hs=%)
OUTS = $(wildcard T*.out)

SHELL = bash

all : run check

run : $(TASKS:%=%.run)

check : $(OUTS:%.out=%.check)

$(TASKS:%=%.bin) : %.bin : %.hs
	@ghc -dynamic -O2 $< -o $@ -main-is $(<:%.hs=%).main	

$(TASKS:%=%.run) : %.run : %.bin %.in
	@echo "Task $(@:T%.run=%):"
	@cat $(<:%.bin=%.in) | ./$<
	@echo

$(TASKS:%=%.test) : %.test : %.bin %.test.in
	@echo "Test $(@:T%.test=%):"
	@cat $(<:%.bin=%.test.in) | ./$<
	@echo

$(OUTS:%.out=%.check) : %.check : %.bin %.in %.out
	@printf "Check $(@:T%.check=%): "
	@diff -us <(cat $(<:%.bin=%.in) | ./$<) $(<:%.bin=%.out)

$(TASKS:%=%.bench) : %.bench : %.bin %.in
	@echo "Bench $(<:T%.hs=%):"
	cat $(<:%.bin=%.in) | time -f "%es" ./$(<:%.hs=%)

.PHONY: %.run %.check
