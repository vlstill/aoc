TASKS_HS = $(wildcard T*.hs)
TASKS_PY = $(wildcard T*.py)
TASKS_CPP = $(wildcard T*.cpp)
TASKS_KT = $(wildcard T*.kt)
TASKS = $(TASKS_HS:%.hs=%) $(TASKS_PY:%.py=%) $(TASKS_CPP:%.cpp=%) $(TASKS_KT:%.kt=%)
OUTS = $(wildcard T*.out.check)
PWD != pwd

SHELL = bash

all : run check

run : $(TASKS:%=%.run)

check : $(OUTS:%.out.check=%.check)

$(TASKS_HS:%.hs=%) : % : %.hs
	@ghc -dynamic -O2 $< -o $@ -main-is $(<:%.hs=%).main

$(TASKS_CPP:%.cpp=%) : % : %.cpp
	@g++ -std=c++20 -O2 -g -Werror=uninitialized $< -o $@

$(TASKS_KT:%.kt=%) : % : %.kt
	@kotlinc $< -include-runtime -d $@.jar
	@printf '#!/bin/sh\njava -jar $@.jar "$$@"\n' > $@
	@chmod +x $@
	@[ -f $@.out.check ] && touch $@.out.check

$(TASKS_PY:%.py=%) : % : %.py
	@printf '#!/bin/sh\nPYTHONPATH=$(PWD)/../fja python3 $<\n' > $@
	@chmod +x $@

$(TASKS:%=%.run) : %.run : % %.in
	@echo "Task $(@:T%.run=%):"
	@cat $<.in | ./$<
	@echo

$(TASKS:%=%.test) : %.test : % %.test.in
	@echo "Test $(@:T%.test=%):"
	@cat $<.test.in | ./$<
	@echo

$(OUTS:%.out.check=%.check) : %.check : % %.in %.out.check
	@printf "Check $(@:T%.check=%): "
	@diff -us <(cat $<.in | ./$< 2>/dev/null) $(<:%=%.out.check)

$(TASKS:%=%.bench) : %.bench : % %.in
	@echo "Bench $(<:T%.hs=%):"
	cat $<.in | time -f "%es" ./$(<:%.hs=%)

clean :
	rm -f *.o *.hi

.PHONY: %.run %.check %.bench %.test clean
.SUFFIXES:
