SHELL=/bin/bash # Required for brace expansion to work
LANGS := rust,julia,haskell

# Output language versions
.PHONY: versions
versions:
	@(rustc --version && cargo --version) 2>/dev/null
	@julia -version 2>/dev/null
	@(stack --version) 2>/dev/null

.PHONY: wildchild/%
wildchild/%:
	echo 'dir=$(*D) & filename=$(@F)'


start/%: start/rust/$*

# Start a new project
.PHONY: start/rust/%
start/rust/%:
	mkdir -p $* && cargo new --name $(@F) $*/rust

start/haskell/%:
	 $(*D)/haskell && cd $(*D)/haskell && stack new --bare $(@F)

.PHONY: run/%
run/%:
	# Rust
	cd $*/rust && cargo run
	# Julia
	# Haskell

.PHONY: build/% build/rust/%
build/%: build/rust/%
	@echo "Delegatin'"

build/rust/%: %/rust/src/*.rs
	cd $*/rust/ && cargo build

.PHONY:  test/%
test/%:
	cd $*/rust/ && cargo test

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
