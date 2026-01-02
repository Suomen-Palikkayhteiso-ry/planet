.PHONY: help all build run clean test repl check

default: help

all: build ## Build the project

build: ## Build the executable
	cabal update
	cabal build
	cp $$(cabal list-bin planet) ./planet

develop: ## Launch opinionated IDE
	devenv --profile full-vim shell -- code .

run: ## Run the planet generator to update site
	cabal run

run-bin: ## Run the compiled executable directly
	./planet

clean: ## Clean build artifacts, output, and test artifacts
	cabal clean
	rm -rf public planet .hpc *.html src/Main

test: ## Run tests
	cabal test

repl: ## Start the REPL
	cabal repl

check: ## Check the package for common errors
	cabal check

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
