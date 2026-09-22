///|
pub fn minify(src : String) -> String raise ParseError {
  let prog = parse_program(src)
  resolve(prog)
  mangle(prog, false)
  print_program(prog)
}

///|
pub fn minify_with(
  src : String,
  rename_top_level : Bool,
) -> String raise ParseError {
  let prog = parse_program(src)
  resolve(prog)
  mangle(prog, rename_top_level)
  print_program(prog)
}
