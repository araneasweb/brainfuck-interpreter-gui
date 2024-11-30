.PHONY: clean
clean:
	stack clean --full

.PHONY: build
build:
	stack build --test --no-run-tests

.PHONY: test
test:
	stack build && bash ./test/shellTest.sh

.PHONY: ghci
ghci:
	stack ghci

.PHONY: gui
gui:
	stack build && stack exec brainfuck-interpreter-gui-exe -- -g

.PHONY: gui-raw
gui-raw:
	stack exec brainfuck-interpreter-gui-exe -- -g

.PHONY: docs
docs:
	stack haddock --open

.PHONY: deps
deps:
	stack build --copy-compiler-tool hlint stylish-haskell

.PHONY: format
format:
	find . -name '*.hs' | xargs -t stack exec -- stylish-haskell -i

.PHONY: lint
lint:
	stack exec -- hlint -i 'Parse error' -i 'Reduce duplication' -i 'Use <=<' src
