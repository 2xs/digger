DIGGER := ./digger

# Generic targets, building everything
default: default_message build_stack

default_message:
	@echo 'Building with `stack` by default (`stack` is usually more reliable than'
	@echo '`cabal`); if this fails, try "make build_cabal" (or installing'
	@echo '`stack`).'
	@echo

build_stack:
	stack build
	ln -sf $$(stack exec which digger) digger

build_cabal:
	cabal build
	ln -sf dist/build/digger/digger digger

clean:
	stack clean || true
	cabal clean || true
	rm -rf digger .stack-work dist

.PHONY: default default_message build_stack build_cabal clean

# Default to stack for precise targets
digger: app/digger.hs $(wildcard src/Coq/*.hs)
	stack build digger:exe:digger
	ln -sf $$(stack exec which digger) digger

# Documentation
doc:
	stack haddock

.PHONY: doc
