SHELL:=/bin/bash -O globstar # Required for brace expansion to work
LANGS := rust,julia,haskell

# Output language versions
.PHONY: versions
versions:
	@(rustc --version && cargo --version) 2>/dev/null
	@julia -version 2>/dev/null
	@(stack --version) 2>/dev/null

start/%: start/rust/$*

# Start a new project
.PHONY: start/rust/%
start/rust/%:
	mkdir -p $* && cargo new --name $(@F) $*/rust

start/haskell/%:
	mkdir -p $*/haskell && cd $(*D)/haskell && stack new $(@F) --bare simple-library

# Compile the code
.PHONY: build/% build/rust/%
build/%: build/rust/%
	@echo "Delegatin'"

build/rust/%: %/rust/src/*.rs
	cd $*/rust/ && cargo build

# Run the tests
.PHONY: test test/rust test/haskell test/julia
test: test/haskell test/rust test/julia

test/haskell:
	@echo "Haskell"
	@find . -name stack.yaml -type f -print -execdir stack test --no-cabal-verbose \;

test/rust:
	@echo "Rust"
	@find . -name Cargo.toml -type f -print -execdir cargo test \;

test/julia:
	@echo Julia
	@for t in **/julia/tests/main.jl; do julia "$$t"; done

# Setup the language directories under a (optionally new) folder
.PHONY: mkdirs/%
mkdirs/%:
	mkdir -p $*/{$(LANGS)}
	# TODO Setup Rust folder using 'cargo init $(@F) --bin'

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
