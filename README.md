# evol

A JavaScript mangler/compressor for MoonBit, built for the MoonBit JS backend.

This repository is a MoonBit workspace with two modules:

- [`minifier/`](./minifier) — the `moonbit-community/evol-minifier` library:
  parse modern JavaScript, resolve scopes, rename local identifiers, and print
  minimal equivalent output.
- [`evol/`](./evol) — the `moonbit-community/evol` command-line program: read
  a file (or stdin) and write the minified result.

`evol` removes unnecessary whitespace and shortens identifier names.

The CLI exits with a nonzero status on input or argument errors. `--help` and `--version` exit successfully.

## CLI options

- `--top`: preserve top-level binding names while renaming local bindings.

```sh
evol --top input.js
```
