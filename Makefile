TASKS_HS = $(wildcard T*.hs)
TASKS = $(TASKS_HS:%.hs=%)
OUTS = $(wildcard T*.out)

SHELL = bash

all : run check

run : $(TASKS:%=%.run)

check : $(OUTS:%.out=%.check)

$(TASKS:%=%.run) : %.run : %.hs %.in
	@echo "Task $(<:T%.hs=%):"
	@cat $(<:%.hs=%.in) | runhaskell $<
	@echo

$(OUTS:%.out=%.check) : %.check : %.hs %.in %.out
	@printf "Check $(<:T%.hs=%): "
	@diff -us <(cat $(<:%.hs=%.in) | runhaskell $<) $(<:%.hs=%.out)

.PHONY: %.run %.check
