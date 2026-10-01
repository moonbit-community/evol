# evol

A command-line minifier for MoonBit's JavaScript output.

```sh
evol input.js
evol input.js --output output.js
evol input.js --output -
```

- `--top`: preserve top-level binding names.
- `--output <path>`: write to the specified file; use `-` for stdout.
  When omitted or empty, `input.js` produces `input.min.js`.

Omit the input file to read stdin, and specify `--output` to select the destination.
