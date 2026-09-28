///|
/// Removes unnecessary whitespace and renames local bindings.
/// Top-level names and names exposed to dynamic scope are preserved.
/// Raises ParseError when the input cannot be parsed.
pub fn minify(src : String) -> String raise ParseError {
  let prog = parse_program(src)
  resolve(prog)
  mangle(prog, false)
  print_program(prog)
}

///|
/// Minifies JavaScript, optionally renaming top-level bindings.
/// Public export names are preserved in either mode.
/// Raises ParseError when the input cannot be parsed.
pub fn minify_with(
  src : String,
  rename_top_level : Bool,
) -> String raise ParseError {
  let prog = parse_program(src)
  resolve(prog)
  mangle(prog, rename_top_level)
  print_program(prog)
}
