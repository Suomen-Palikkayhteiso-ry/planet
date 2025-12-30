.PHONY: help all build run clean test repl check

default: help

all: build ## Build the project

build: ## Build the executable
	cabal build

run: ## Run the planet generator to update site
	cabal run

clean: ## Clean build artifacts and output
	cabal clean
	rm -rf public

test: ## Run tests
	cabal test

repl: ## Start the REPL
	cabal repl

check: ## Check the package for common errors
	cabal check

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
