SHELL=/bin/bash # Required for brace expansion to work
LANGS := rust,julia,haskell
# Output language versions
.PHONY: version
version:
	rustc --version && cargo --version

# Setup the language directories under a (optionally new) folder
.PHONY: mkdirs/%
mkdirs/%:
	mkdir -p $*/{$(LANGS)}
	# TODO Setup Rust folder using 'cargo init $(@F) --bin'

.PHONY: clean
clean:
	find . -name main -delete
