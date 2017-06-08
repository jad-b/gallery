SHELL:=/bin/bash -O globstar # Required for brace expansion to work
LANGS := rust,julia,haskell

# Output language versions
.PHONY: versions
versions:
	@(rustc --version && cargo --version) 2>/dev/null
	@julia -version 2>/dev/null
	@(stack --version) 2>/dev/null

# Compile the code
.PHONY: build/% build/rust/%
build/%: build/rust/%
	@echo "Delegatin'"

build/rust/%:
	cd rust/ && cargo build

# Run the tests
.PHONY: test test/rust test/haskell test/julia
test: test/haskell test/rust test/julia

test/haskell:
	@echo "Haskell"
	@cd haskell/ && stack test --no-cabal-verbose

test/rust:
	@echo "Rust"
	@cd rust/ && cargo test

test/julia:
	@echo Julia
	@julia "./julia/tests/main.jl"

.PHONY: clean
clean:
	find . -name main -delete

# Experimenting with Make's automatic variables
.PHONY: autovar
autovar/static/%:
	@echo '$$@=$@'
	@echo '$$%=$%'
	@echo '$$<=$<'
	@echo '$$?=$?'
	@echo '$$^=$^'
	@echo '$$+=$+'
	@echo '$$|=$|'
	@echo '$$*=$*'
	@echo '$$(@D)=$(@D)'
	@echo '$$(@F)=$(@F)'
	@echo '$$(*D)=$(*D)'
	@echo '$$(*F)=$(*F)'
	@echo '$$(%D)=$(%D)'
	@echo '$$(%F)=$(%F)'
	@echo '$$(<D)=$(<D)'
	@echo '$$(<F)=$(<F)'
	@echo '$$(^D)=$(^D)'
	@echo '$$(^F)=$(^F)'
	@echo '$$(+D)=$(+D)'
	@echo '$$(+F)=$(+F)'
	@echo '$$(?D)=$(?D)'
	@echo '$$(?F)=$(?F)'
