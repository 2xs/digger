default: default_message build_stack

default_message:
	@echo 'Building with `stack` by default (`stack` is usually more reliable than'
	@echo '`cabal`); if this fails, try "make build_cabal" (or installing'
	@echo '`stack`).'
	@echo

build_stack:
	stack build
	ln -sf $$(find .stack-work/install/ -name coq2c -type f) coq2c

build_cabal:
	cabal build
	ln -sf dist/build/coq2c/coq2c coq2c

clean:
	stack clean || true
	cabal clean || true
	rm -rf coq2c .stack-work dist

.PHONY: default default_message build_stack build_cabal clean
