# Run the test suite during development.
dev:
	stack test --fast --haddock-deps --file-watch

# Autoformat code using Brittany.
format:
	brittany --indent=4 --write-mode=inplace ./**/*.hs

# Copy binaries to $HOME/.local/bin.
install:
	stack install

# Build PDF and write to disk.
%.pdf: %.md
	pandoc --filter pandoc-theorem-exe $< -H examples/header.tex -o $@

# Output LaTeX to stdout.
%.tex: %.md
	pandoc --filter pandoc-theorem-exe $< -H examples/header.tex -t latex

# Output the Pandoc AST to stdout.
%.ast: %.md
	pandoc --filter pandoc-theorem-exe $< -H examples/header.tex -t native

# Build PDF without filter, and write to disk.
%.clean.pdf: %.md
	pandoc $< -o $@

# Output LaTeX to stdout, compiling without the filter.
%.clean.tex: %.md
	pandoc $< -t latex

# Output the Pandoc AST to stdout, compiling without the filter.
%.clean.ast: %.md
	pandoc $< -t native
