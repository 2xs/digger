DIGGER := ../digger

_: Example.o

.PRECIOUS: Example.c

-include .Makefile.deps

Example.json: Extraction.vo

.Makefile.deps: $(wildcard *.v)
	coqdep $^ > $@

%.vo: %.v
	coqc $<

%.c: %.json
	$(DIGGER) -m Monad -M coq_M -R returnM -B bindM                   \
	    -m Datatypes -r Coq_true:true -r Coq_false:false -r Coq_tt:tt \
	    -q base.h -q Monad.h -q Internals.h                           \
	    -m Internals -d :Internals.json                               \
	    -o $@ $<

%.o: %.c
	cc -Wall -Wextra -c $<

clean:
	rm -f .Makefile.deps *.vo *.aux .*.vo.aux *.glob *.o *.json Example.c
