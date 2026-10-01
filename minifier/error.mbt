///|
/// A 1-based source position, counted in UTF-16 code units.
pub struct Position {
  line : Int
  column : Int
}

///|
/// A failure to parse or print JavaScript source.
///
/// Each variant carries the data needed to describe the failure, and `Show`
/// renders the user-facing message.
pub suberror ParseError {
  /// A token of `expected` was required, but `got` appeared.
  UnexpectedToken(Position, String, String)
  /// The lexer met a character that cannot start a token.
  UnexpectedCharacter(Position, Char)
  /// The input ends inside an unterminated comment, string, template or regexp.
  Unterminated(Position, String)
  /// A malformed escape sequence.
  InvalidEscape(Position, String)
  /// Any other syntax error, described by `message`.
  Syntax(Position, String)
}

///|
/// Computes the 1-based line and column of a UTF-16 code-unit `offset`.
fn position_at(source : String, offset : Int) -> Position {
  let length = source.length()
  let limit = if offset < length { offset } else { length }
  for index = 0, line = 1, column = 1 {
    guard index < limit else { break { line, column, } }
    let character = source[index]
    guard is_line_terminator(character) else {
      continue index + 1, line, column + 1
    }
    let is_crlf = character == '\r' &&
      index + 1 < limit &&
      source[index + 1] == '\n'
    let next = index + (if is_crlf { 2 } else { 1 })
    continue next, line + 1, 1
  }
}

///|
fn position_text(position : Position) -> String {
  " at \{position.line}:\{position.column}"
}

///|
pub impl Show for ParseError with fn output(self, logger) {
  match self {
    UnexpectedToken(position, expected, got) =>
      logger.write("expected \{expected}, got \{got}\{position_text(position)}")
    UnexpectedCharacter(position, character) =>
      logger.write(
        "unexpected character '\{character.escape(quote=false)}'\{position_text(position)}",
      )
    Unterminated(position, construct) =>
      logger.write("unterminated \{construct}\{position_text(position)}")
    InvalidEscape(position, message) =>
      logger.write("\{message}\{position_text(position)}")
    Syntax(position, message) =>
      logger.write("\{message}\{position_text(position)}")
  }
}

///|
#deprecated
pub extend ParseError with Show::{to_string, output}
