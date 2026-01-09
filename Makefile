.PHONY: help all build build-all run clean test repl check cabal-check watch

default: help

all: build-all ## Build the project

build-all: build ## Build the entire project
	cabal run
	cd elm-app && npm run build

build: ## Build the executable
	cabal update
	cabal build
	cp $(shell cabal list-bin planet) ./planet

develop: ## Launch opinionated IDE
	devenv --profile full-vim shell -- code .

run: ## Run the planet generator to update site
	cabal run

run-bin: ## Run the compiled executable directly
	./planet

clean: ## Clean build artifacts, output, and test artifacts
	cabal clean
	rm -rf public planet .hpc *.html src/Main

test: check ## Run tests
	cabal test

repl: ## Start the REPL
	cabal repl

check: ## Run hlint static analysis
	hlint src tests

cabal-check: ## Check the package for common errors
	cabal check

watch: ## Watch for changes in Haskell and Elm files and rebuild
	make run-bin
	find src planet.cabal planet.toml -name "*.hs" -o -name "*.cabal" -o -name "*.toml" | entr -s 'make run-bin' &
	cd elm-app && npm run dev

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
