.PHONY: test build docs package package-list lint

# Run tests
test:
	cabal test

# Build the project
build:
	cabal build

# Generate documentation for Hackage
docs:
	cabal haddock --haddock-for-hackage --enable-doc

# Open the generated documentation in the browser
docs-open: docs
	cabal haddock --open

# Create source distribution package
package:
	cabal sdist

# List files that would be included in the package
package-list:
	cabal sdist --list-only

# Run HLint recursively on source files
lint:
	hlint .

# Default target
all: build test 