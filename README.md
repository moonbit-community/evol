# evol

A JavaScript mangler/compressor for MoonBit, built for the MoonBit JS backend.

This repository is a MoonBit workspace with two modules:

- [`minifier/`](./minifier) — the `moonbit-community/evol-minifier` library:
  parse modern JavaScript, resolve scopes, rename local identifiers, and print
  minimal equivalent output.
- [`evol/`](./evol) — the `moonbit-community/evol` command-line program: read
  a file (or stdin) and write the minified result.

## Scope

`evol` intentionally does no dead-code elimination or other AST transforms;
the MoonBit compiler already emits tight code. The focus is whitespace removal
and local identifier mangling, which account for most of the size reduction.

The CLI exits with a nonzero status on input or argument errors. `--help` and
`--version` exit successfully.
