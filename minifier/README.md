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
- `minify_with(src, true)` also renames top-level bindings.

## Design

- `parser.mbt` builds a flat node arena and records scope declarations and
  identifier references.
- `cover.mbt` parses ambiguous parentheses and destructuring once, then converts
  binding positions when an arrow or assignment establishes their meaning.
  `class_members.mbt` handles contextual class modifiers without parser probes.
- Identifier spellings are interned; escaped and literal spellings share integer
  name identities for scope lookup.
- Small immutable records use `#valtype` to avoid individual heap allocations
  on native targets. Reference, symbol and scope updates replace arena rows by
  ID. Scope member maps and generated name strings live in separate Program
  tables, so the hot records contain only scalar fields.
- `modules.mbt` parses and prints imports and exports, keeping public names
  separate from local bindings.
- `resolve.mbt` collapses provisional expression scopes and binds references.
- `mangle.mbt` reuses short names in noninterfering scopes. Captured references,
  declaration locations, parameters and linked catch/var bindings constrain
  reuse, preventing capture and accidental binding merges.
- `printer.mbt` renames identifiers and prints with minimal whitespace while
  preserving original parentheses and literal spelling.

Direct `eval` and `with` preserve identifier spellings. Bindings named `arguments`
and block function names that may participate in Annex B semantics are also
preserved. Top-level renaming keeps directly exported declarations and public
export aliases intact.
