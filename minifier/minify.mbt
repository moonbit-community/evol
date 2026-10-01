///|
/// Removes unnecessary whitespace and renames local and top-level bindings.
/// Public export names and names exposed to dynamic scope are preserved.
/// internal_properties explicitly selects internal ASCII property names.
/// These properties must not be part of external object interfaces or accessed
/// through dynamically computed names. Other properties remain unchanged.
/// Raises ParseError when the input cannot be parsed.
pub fn minify(
  source : String,
  internal_properties? : ArrayView[String] = [],
) -> String raise ParseError {
  minify_with(source, true, internal_properties~)
}

///|
/// Minifies JavaScript, optionally renaming top-level bindings.
/// Public export names are preserved in either mode.
/// internal_properties has the same ownership requirements as minify.
/// Raises ParseError when the input cannot be parsed.
pub fn minify_with(
  source : String,
  rename_top_level : Bool,
  internal_properties? : ArrayView[String] = [],
) -> String raise ParseError {
  let program = Program::parse(source)
  program.resolve()
  program.prepare_properties(internal_properties)
  program.mangle(rename_top_level)
  program.mangle_properties()
  program.print()
}
