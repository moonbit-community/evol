# evol-minifier

The `moonbit-community/evol-minifier` library: a JavaScript mangler/compressor
for MoonBit, designed for the MoonBit JS backend. It parses modern JavaScript, resolves scopes, renames local
identifiers, and prints the smallest equivalent output. It intentionally does
no dead-code elimination or other AST transforms: the MoonBit compiler already
emits tight code.
