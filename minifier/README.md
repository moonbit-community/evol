# evol-minifier

The `moonbit-community/evol-minifier` library: a JavaScript mangler/compressor
for MoonBit, designed for the MoonBit JS backend. It parses modern JavaScript, resolves scopes, renames local
identifiers, and prints the smallest equivalent output. It intentionally does
no dead-code elimination or other AST transforms: the MoonBit compiler already
emits tight code.

## Usage

```moonbit
let out = @minifier.minify(src)
```

- `minify(src)` keeps top-level names unchanged.
- `minify_with(src, rename_top_level=true)` also renames top-level bindings.

## Design

- `parser.mbt` builds a flat node arena and records scope declarations and
  identifier references.
- `resolve.mbt` binds references in one pass over the flat reference table.
- `mangle.mbt` assigns short names to local symbols.
- `printer.mbt` renames identifiers and prints with minimal whitespace while
  preserving original parentheses and literal spelling.
