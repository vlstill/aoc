TASKS_HS = $(wildcard T*.hs)
TASKS_PY = $(wildcard T*.py)
TASKS_CPP = $(wildcard T*.cpp)
TASKS_KT = $(wildcard T*.kt)
TASKS_GO = $(wildcard T*.go)
TASKS = $(TASKS_HS:%.hs=%) $(TASKS_PY:%.py=%) $(TASKS_CPP:%.cpp=%) $(TASKS_KT:%.kt=%) $(TASKS_GO:%.go=%)
OUTS = $(wildcard T*.out.check)
PWD != pwd

SHELL = bash

all : run check

run : $(TASKS:%=%.run)

check : $(OUTS:%.out.check=%.check)

%.in : %.sed %.in.raw
	@sed -f $^ > $@

%.test.in : %.sed %.test.in.raw
	@sed -f $^ > $@

$(TASKS_HS:%.hs=%) : % : %.hs
	@flock $< ghc -dynamic -O2 $< -o $@ -main-is $(<:%.hs=%).main

$(TASKS_CPP:%.cpp=%) : % : %.cpp
	@flock $< g++ -std=c++2y -O2 -g -Werror=uninitialized $< -o $@

$(TASKS_KT:%.kt=%) : % : %.kt
	@kotlinc $< -include-runtime -d $@.jar
	@printf '#!/bin/sh\njava -jar $@.jar "$$@"\n' > $@
	@chmod +x $@
	@[ -f $@.out.check ] && touch $@.out.check

$(TASKS_GO:%.go=%) : % : %.go
	@flock $< go build $<

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
