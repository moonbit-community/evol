///|
priv struct Lexer {
  source : String
  // The next unread UTF-16 code unit; token spans are [start, end).
  mut position : Int
  mut token : Token
  mut keyword : Keyword
  mut start : Int
  mut end : Int
  mut newline_before : Bool
  mut quasi_start : Int
  mut quasi_end : Int
}

///|
fn Lexer::new(source : String) -> Lexer {
  {
    source,
    position: 0,
    token: EndOfFile,
    keyword: NotKeyword,
    start: 0,
    end: 0,
    newline_before: false,
    quasi_start: 0,
    quasi_end: 0,
  }
}

///|
#valtype
priv struct LexerSnapshot {
  position : Int
  token : Token
  keyword : Keyword
  start : Int
  end : Int
  newline_before : Bool
  quasi_start : Int
  quasi_end : Int
}

///|
fn Lexer::snapshot(self : Lexer) -> LexerSnapshot {
  {
    position: self.position,
    token: self.token,
    keyword: self.keyword,
    start: self.start,
    end: self.end,
    newline_before: self.newline_before,
    quasi_start: self.quasi_start,
    quasi_end: self.quasi_end,
  }
}

///|
fn Lexer::restore(self : Lexer, other : LexerSnapshot) -> Unit {
  self.position = other.position
  self.token = other.token
  self.keyword = other.keyword
  self.start = other.start
  self.end = other.end
  self.newline_before = other.newline_before
  self.quasi_start = other.quasi_start
  self.quasi_end = other.quasi_end
}

///|
fn Lexer::error_position(self : Lexer) -> Position {
  position_at(self.source, self.start)
}

///|
fn Lexer::syntax_error(self : Lexer, message : String) -> ParseError {
  Syntax(self.error_position(), message)
}

///|
fn Lexer::unterminated(self : Lexer, construct : String) -> ParseError {
  Unterminated(self.error_position(), construct)
}

///|
fn Lexer::invalid_escape(self : Lexer, message : String) -> ParseError {
  InvalidEscape(self.error_position(), message)
}

///|
const LINE_TERMINATORS : Regex = re"[\n\r\u2028\u2029]+"

///|
// ASCII whitespace is scanned directly; preserve the non-ASCII set here.
const NON_ASCII_WHITESPACE : Regex = re"[\u0085\u00A0\u1680\u2000-\u200A\u202F\u205F\u3000\uFEFF]+"

///|
const DECIMAL_MANTISSA : Regex = re"[0-9][0-9_]*(?:\.[0-9_]*)?" |
  re"\.[0-9][0-9_]*"

///|
// Preserve the existing permissive spellings, including incomplete exponents.
const NUMBER_SPELLING : Regex = re"0[xX][0-9a-fA-F_]*n?" |
  re"0[bB][01_]*n?" |
  re"0[oO][0-7_]*n?" |
  (DECIMAL_MANTISSA + re"(?:[eE][+\-]?[0-9_]*)?n?")

///|
// Only ill-formed UTF-16 can interrupt the regex before the end of a line.
fn Lexer::line_comment_tail(self : Lexer) -> Unit {
  while self.position < self.source.length() &&
        self.source.get_char(self.position) is None {
    self.position += 1
    lexmatch self.source.view(start_offset=self.position) {
      (re"^[^\n\r\u2028\u2029]*", after=rest) =>
        self.position = rest.start_offset()
    }
  }
}

///|
fn Lexer::block_comment_tail(
  self : Lexer,
  comment_start : Int,
) -> Unit raise ParseError {
  while true {
    lexmatch self.source.view(start_offset=self.position) with longest {
      (re"^\*+/", after=rest) => {
        self.position = rest.start_offset()
        return
      }
      (re"^" + LINE_TERMINATORS, after=rest) => {
        self.newline_before = true
        self.position = rest.start_offset()
      }
      (re"^[^*\n\r\u2028\u2029]+", after=rest) =>
        self.position = rest.start_offset()
      (re"^\*+", after=rest) => self.position = rest.start_offset()
      _ => {
        guard self.position < self.source.length() else {
          raise Unterminated(position_at(self.source, comment_start), "comment")
        }
        self.position += 1
      }
    }
  }
}

///|
fn Lexer::finish_token(self : Lexer, token : Token, rest : StringView) -> Unit {
  self.token = token
  self.position = rest.start_offset()
  self.end = self.position
}

///|
fn Lexer::finish_single_character_token(self : Lexer, token : Token) -> Unit {
  self.token = token
  self.position += 1
  self.end = self.position
}

///|
fn Lexer::finish_identifier(
  self : Lexer,
  rest : StringView,
) -> Unit raise ParseError {
  guard rest.length() == 0 || rest.get_char(0) is Some(_) else {
    self.position = rest.start_offset()
    return self.identifier_tail(self.start)
  }
  let spelling = self.source.view(
    start_offset=self.start,
    end_offset=rest.start_offset(),
  )
  self.keyword = keyword_of(spelling)
  self.finish_token(Identifier, rest)
}

///|
fn Lexer::skip_ascii_whitespace(self : Lexer) -> Unit {
  self.position = for position = self.position {
    guard position < self.source.length() else { break position }
    match self.source[position] {
      ' ' | '\t' | '\u000B' | '\u000C' => continue position + 1
      '\n' | '\r' => {
        self.newline_before = true
        continue position + 1
      }
      _ => break position
    }
  }
}

///|
/// Called with position immediately after the comment marker.
fn Lexer::line_comment(self : Lexer) -> Unit {
  lexmatch self.source.view(start_offset=self.position) {
    (re"^[^\n\r\u2028\u2029]*", after=rest) =>
      self.position = rest.start_offset()
  }
  self.line_comment_tail()
}

///|
fn Lexer::next(self : Lexer) -> Unit raise ParseError {
  self.newline_before = false
  self.keyword = NotKeyword
  let source = self.source
  let length = source.length()
  while true {
    self.start = self.position
    guard self.position < length else {
      return self.finish_token(EndOfFile, source.view(start_offset=length))
    }
    let character = source[self.position]
    match character {
      ' ' | '\t' | '\u000B' | '\u000C' | '\n' | '\r' => {
        self.skip_ascii_whitespace()
        continue
      }
      'A'..='Z' | 'a'..='z' | '_' | '$' =>
        return self.identifier_token(
          source.view(start_offset=self.position + 1),
        )
      '0'..='9' => return self.number_token()
      '.' => {
        let next = self.position + 1
        if next < length && source[next] >= '0' && source[next] <= '9' {
          return self.number_token()
        }
        return self.operator_token()
      }
      '/' => {
        let next = self.position + 1
        if next < length && source[next] == '/' {
          self.position = next + 1
          self.line_comment()
          continue
        }
        if next < length && source[next] == '*' {
          let comment_start = self.position
          self.position = next + 1
          self.block_comment_tail(comment_start)
          continue
        }
        return self.operator_token()
      }
      '#' => {
        let next = self.position + 1
        if self.position == 0 && next < length && source[next] == '!' {
          self.position = next + 1
          self.line_comment()
          continue
        }
        self.position = next
        return self.identifier_tail(next)
      }
      '\\' => return self.identifier_tail(self.position)
      '\'' => return self.string('\'')
      '"' => return self.string('"')
      '`' => {
        self.position += 1
        return self.template_chunk(true)
      }
      '(' => return self.finish_single_character_token(LeftParenthesis)
      ')' => return self.finish_single_character_token(RightParenthesis)
      '[' => return self.finish_single_character_token(LeftBracket)
      ']' => return self.finish_single_character_token(RightBracket)
      '{' => return self.finish_single_character_token(LeftBrace)
      '}' => return self.finish_single_character_token(RightBrace)
      ';' => return self.finish_single_character_token(Semicolon)
      ',' => return self.finish_single_character_token(Comma)
      ':' => return self.finish_single_character_token(Colon)
      '~' => return self.finish_single_character_token(Tilde)
      '\u0000'..='\u007F' => return self.operator_token()
      _ =>
        lexmatch source.view(start_offset=self.position) {
          (re"^[\u2028\u2029]+", after=rest) => {
            self.newline_before = true
            self.position = rest.start_offset()
            continue
          }
          (re"^" + NON_ASCII_WHITESPACE, after=rest) => {
            self.position = rest.start_offset()
            continue
          }
          (re"^" + RAW_IDENTIFIER_START, after=rest) =>
            return self.identifier_token(rest)
          _ => return self.operator_token()
        }
    }
  }
}

///|
fn Lexer::number_token(self : Lexer) -> Unit {
  lexmatch self.source.view(start_offset=self.position) with longest {
    (re"^" + NUMBER_SPELLING, after=rest) => self.finish_token(Number, rest)
    _ => abort("number dispatch requires a number prefix")
  }
}

///|
fn ascii_identifier_rest(input : StringView) -> StringView {
  // position is the first unread UTF-16 code unit within input.
  let end = for position = 0 {
    guard position < input.length() else { break position }
    match input[position] {
      'A'..='Z' | 'a'..='z' | '0'..='9' | '_' | '$' | '#' =>
        continue position + 1
      _ => break position
    }
  }
  input.view(start_offset=end)
}

///|
fn Lexer::identifier_token(
  self : Lexer,
  tail : StringView,
) -> Unit raise ParseError {
  // MoonBit's generated names have long ASCII runs. Scan those directly, then
  // handle names that continue through an escape or a non-ASCII character.
  let rest = ascii_identifier_rest(tail)
  lexmatch rest {
    (re"^\\" | re"^" + RAW_IDENTIFIER_START, after=_) => {
      self.position = rest.start_offset()
      self.identifier_tail(self.start)
    }
    _ => self.finish_identifier(rest)
  }
}

///|
fn Lexer::operator_token(self : Lexer) -> Unit raise ParseError {
  lexmatch self.source.view(start_offset=self.position) with longest {
    (re"^\?", after=rest) => return self.finish_token(Question, rest)
    (re"^\?\?", after=rest) => return self.finish_token(NullishCoalescing, rest)
    (re"^\?\?=", after=rest) =>
      return self.finish_token(NullishCoalescingAssignment, rest)
    (re"^\.", after=rest) => return self.finish_token(Dot, rest)
    (re"^\.\.\.", after=rest) => return self.finish_token(Ellipsis, rest)
    (re"^=", after=rest) => return self.finish_token(Assignment, rest)
    (re"^=>", after=rest) => return self.finish_token(Arrow, rest)
    (re"^==", after=rest) => return self.finish_token(Equal, rest)
    (re"^===", after=rest) => return self.finish_token(StrictlyEqual, rest)
    (re"^!", after=rest) => return self.finish_token(LogicalNot, rest)
    (re"^!=", after=rest) => return self.finish_token(NotEqual, rest)
    (re"^!==", after=rest) => return self.finish_token(StrictlyNotEqual, rest)
    (re"^<", after=rest) => return self.finish_token(LessThan, rest)
    (re"^<=", after=rest) => return self.finish_token(LessThanOrEqual, rest)
    (re"^<<", after=rest) => return self.finish_token(ShiftLeft, rest)
    (re"^<<=", after=rest) =>
      return self.finish_token(LeftShiftAssignment, rest)
    (re"^>", after=rest) => return self.finish_token(GreaterThan, rest)
    (re"^>=", after=rest) => return self.finish_token(GreaterThanOrEqual, rest)
    (re"^>>", after=rest) => return self.finish_token(ShiftRight, rest)
    (re"^>>=", after=rest) =>
      return self.finish_token(RightShiftAssignment, rest)
    (re"^>>>", after=rest) => return self.finish_token(UnsignedShiftRight, rest)
    (re"^>>>=", after=rest) =>
      return self.finish_token(UnsignedRightShiftAssignment, rest)
    (re"^&", after=rest) => return self.finish_token(BitwiseAnd, rest)
    (re"^&=", after=rest) =>
      return self.finish_token(BitwiseAndAssignment, rest)
    (re"^&&", after=rest) => return self.finish_token(LogicalAnd, rest)
    (re"^&&=", after=rest) =>
      return self.finish_token(LogicalAndAssignment, rest)
    (re"^\|", after=rest) => return self.finish_token(BitwiseOr, rest)
    (re"^\|=", after=rest) =>
      return self.finish_token(BitwiseOrAssignment, rest)
    (re"^\|\|", after=rest) => return self.finish_token(LogicalOr, rest)
    (re"^\|\|=", after=rest) =>
      return self.finish_token(LogicalOrAssignment, rest)
    (re"^\^", after=rest) => return self.finish_token(BitwiseXor, rest)
    (re"^\^=", after=rest) =>
      return self.finish_token(BitwiseXorAssignment, rest)
    (re"^\+", after=rest) => return self.finish_token(Plus, rest)
    (re"^\+\+", after=rest) => return self.finish_token(Increment, rest)
    (re"^\+=", after=rest) => return self.finish_token(AdditionAssignment, rest)
    (re"^-", after=rest) => return self.finish_token(Minus, rest)
    (re"^--", after=rest) => return self.finish_token(Decrement, rest)
    (re"^-=", after=rest) =>
      return self.finish_token(SubtractionAssignment, rest)
    (re"^\*", after=rest) => return self.finish_token(Asterisk, rest)
    (re"^\*\*", after=rest) =>
      return self.finish_token(ExponentiationOperator, rest)
    (re"^\*=", after=rest) =>
      return self.finish_token(MultiplicationAssignment, rest)
    (re"^\*\*=", after=rest) =>
      return self.finish_token(ExponentiationAssignment, rest)
    (re"^%", after=rest) => return self.finish_token(Percent, rest)
    (re"^%=", after=rest) => return self.finish_token(ModuloAssignment, rest)
    (re"^/", after=rest) => return self.finish_token(Slash, rest)
    (re"^/=", after=rest) => return self.finish_token(DivisionAssignment, rest)
    // Before a digit, ? starts a conditional and the dot belongs to a number.
    (re"^\?\.[0-9]", after=_) =>
      return self.finish_token(
        Question,
        self.source.view(start_offset=self.position + 1),
      )
    (re"^\?\.", after=rest) => return self.finish_token(OptionalChaining, rest)
    _ => {
      guard self.source.get_char(self.position) is Some(_) else {
        return self.identifier_tail(self.position)
      }
      raise UnexpectedCharacter(
        position_at(self.source, self.position),
        self.source[self.position].to_int().to_char().unwrap_or('?'),
      )
    }
  }
}
