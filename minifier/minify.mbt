///|
/// Removes unnecessary whitespace and renames local bindings.
/// Top-level names and names exposed to dynamic scope are preserved.
/// Raises ParseError when the input cannot be parsed.
pub fn minify(source : String) -> String raise ParseError {
  minify_with(source, false)
}

///|
/// Minifies JavaScript, optionally renaming top-level bindings.
/// Public export names are preserved in either mode.
/// Raises ParseError when the input cannot be parsed.
pub fn minify_with(
  source : String,
  rename_top_level : Bool,
) -> String raise ParseError {
  let program = parse_program(source)
  resolve(program)
  mangle(program, rename_top_level)
  print_program(program)
}
