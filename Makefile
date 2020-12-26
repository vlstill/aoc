TASKS_HS = $(wildcard T*.hs)
TASKS_PY = $(wildcard T*.py)
TASKS_CPP = $(wildcard T*.cpp)
TASKS = $(TASKS_HS:%.hs=%) $(TASKS_PY:%.py=%) $(TASKS_CPP:%.cpp=%)
OUTS = $(wildcard T*.out)
PWD != pwd

SHELL = bash

all : run check

run : $(TASKS:%=%.run)

check : $(OUTS:%.out=%.check)

$(TASKS_HS:%.hs=%) : % : %.hs
	@ghc -dynamic -O2 $< -o $@ -main-is $(<:%.hs=%).main

$(TASKS_CPP:%.cpp=%) : % : %.cpp
	@clang++ -std=c++20 -O2 $< -o $@

$(TASKS_PY:%.py=%) : % : %.py
	@printf '#!/bin/sh\nPYTHONPATH=$(PWD)/fja python3 $<\n' > $@
	@chmod +x $@

$(TASKS:%=%.run) : %.run : % %.in
	@echo "Task $(@:T%.run=%):"
	@cat $<.in | ./$<
	@echo

$(TASKS:%=%.test) : %.test : % %.test.in
	@echo "Test $(@:T%.test=%):"
	@cat $<.test.in | ./$<
	@echo

$(OUTS:%.out=%.check) : %.check : % %.in %.out
	@printf "Check $(@:T%.check=%): "
	@diff -us <(cat $<.in | ./$<) $(<:%=%.out)

$(TASKS:%=%.bench) : %.bench : % %.in
	@echo "Bench $(<:T%.hs=%):"
	cat $<.in | time -f "%es" ./$(<:%.hs=%)

clean :
	rm -f *.o *.hi

.PHONY: %.run %.check %.bench %.test clean
