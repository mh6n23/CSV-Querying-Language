# CQL — A Domain-Specific Language for Querying CSV Files

A small query language for CSV data, implemented in Haskell with Alex and
Happy. Supports relational algebra operations — selection, projection,
three join types, cartesian product, union and difference — composed
into nested expressions.

The full manual, including the BNF grammar, a worked example for every
operation and the complete error message reference, is in
[`CSV Language Guide.pdf`](CSV Language Guide.pdf).

## How it works

A standard three-stage pipeline:

1. **Lexer** (Alex) — tokenises the source program
2. **Parser** (Happy) — builds an abstract syntax tree
3. **Interpreter** — evaluates the tree against the input CSV files,
   writing the result to stdout

Validation covers inconsistent arity across CSV rows, out-of-bounds
column references, arity mismatches in union and difference, malformed
join conditions and missing `.csv` extensions, each with a descriptive
error naming the offending column or file.

## Building and running

```bash
stack build
stack exec csv-language-project-exe -- query.cql
```

CSV files referenced by a program must be in the working directory.

## Editor support

A VS Code extension providing syntax highlighting for `.cql` files is in
[`vscode-extension/`](vscode-extension/).
