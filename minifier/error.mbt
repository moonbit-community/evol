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
    lexmatch source.view(start_offset=index, end_offset=limit) with longest {
      (re"^(?:\r\n|[\n\r\u2028\u2029])", after=rest) =>
        continue rest.start_offset(), line + 1, 1
      (re"^[^\n\r\u2028\u2029]+" as text, after=rest) =>
        continue rest.start_offset(), line, column + text.length()
      // Regex consumes Unicode scalars; a lone UTF-16 unit still occupies a column.
      _ => continue index + 1, line, column + 1
    }
  }
}

///|
fn Position::text(position : Position) -> String {
  " at \{position.line}:\{position.column}"
}

///|
pub impl Show for ParseError with fn output(self, logger) {
  match self {
    UnexpectedToken(position, expected, got) =>
      logger.write("expected \{expected}, got \{got}\{position.text()}")
    UnexpectedCharacter(position, character) =>
      logger.write(
        "unexpected character '\{character.escape(quote=false)}'\{position.text()}",
      )
    Unterminated(position, construct) =>
      logger.write("unterminated \{construct}\{position.text()}")
    InvalidEscape(position, message) =>
      logger.write("\{message}\{position.text()}")
    Syntax(position, message) => logger.write("\{message}\{position.text()}")
  }
}

///|
#deprecated
pub extend ParseError with Show::{to_string, output}
