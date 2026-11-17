BIN := zustral
SRC := lib/*.ml lib/*.mli lib/*.mll lib/*.mly lib/dune cli/*.ml cli/*.mli cli/dune bin/dune bin/zustral.ml lib/BuiltInModules.ml
PREFIX ?= /usr/local

.PHONY: all
all: $(BIN)

lib/BuiltInModules.ml: lib/builtin/*.zui lib/builtin/*.zum lib/prelude.c concat_builtins.py
	python3 concat_builtins.py

$(BIN): $(SRC)
	dune build
	cp _build/default/bin/zustral.exe $(BIN)

.PHONY: test
test: $(BIN)
	dune runtest

# How much of the compiler the programs in the repository execute.
.PHONY: coverage
coverage:
	tools/refactoring/coverage.sh

.PHONY: install
install: $(BIN)
	install -D -m 755 zustral $(PREFIX)/bin/zustral

.PHONY: uninstall
uninstall:
	sudo rm $(PREFIX)/bin/zustral

.PHONY: clean
clean:
	rm -f $(BIN); rm -rf _build; rm -f lib/BuiltInModules.ml
