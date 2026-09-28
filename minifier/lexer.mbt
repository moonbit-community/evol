///|
fn is_line_terminator(c : UInt16) -> Bool {
  c == '\n' || c == '\r' || c == 0x2028 || c == 0x2029
}

///|
fn is_whitespace(c : UInt16) -> Bool {
  if c < 0x80 {
    c == ' ' || c == '\t' || c == 0x0B || c == 0x0C
  } else {
    c == 0xFEFF ||
    c.to_int().to_char().map_or(false, character => character.is_whitespace())
  }
}

///|
fn is_identifier_start(c : UInt16) -> Bool {
  c == '$' ||
  c == '_' ||
  c == '#' ||
  (c >= 'a' && c <= 'z') ||
  (c >= 'A' && c <= 'Z') ||
  c >= 0x80
}

///|
fn is_identifier_continue(c : UInt16) -> Bool {
  is_identifier_start(c) || (c >= '0' && c <= '9')
}

///|
fn is_digit(c : UInt16) -> Bool {
  c >= '0' && c <= '9'
}

///|
fn is_hex(c : UInt16) -> Bool {
  is_digit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
}

///|
fn is_oct(c : UInt16) -> Bool {
  c >= '0' && c <= '7'
}

///|
priv struct Lexer {
  source : String
  mut position : Int
  mut token : Token
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
fn Lexer::character(self : Lexer) -> UInt16 {
  self.character_at(0)
}

///|
fn Lexer::character_at(self : Lexer, offset : Int) -> UInt16 {
  let i = self.position + offset
  if i < 0 || i >= self.source.length() {
    0
  } else {
    self.source[i]
  }
}

///|
fn Lexer::peek(self : Lexer, offset : Int) -> Int {
  let i = self.position + offset
  if i < 0 || i >= self.source.length() {
    -1
  } else {
    self.source[i].to_int()
  }
}

///|
fn Lexer::is_keyword(self : Lexer, keyword : String) -> Bool {
  self.token == Identifier &&
  self.source
  .view(start_offset=self.start, end_offset=self.end)
  .equal_to_string(keyword)
}

///|
fn Lexer::skip_trivia(self : Lexer) -> Unit raise ParseError {
  let length = self.source.length()
  if self.position == 0 &&
    length >= 2 &&
    self.source[0] == '#' &&
    self.source[1] == '!' {
    while self.position < length && !is_line_terminator(self.character()) {
      self.position = self.position + 1
    }
  }
  while self.position < length {
    let c = self.character()
    if is_line_terminator(c) {
      self.newline_before = true
      self.position = self.position + 1
      continue
    }
    if is_whitespace(c) {
      self.position = self.position + 1
      continue
    }
    if c == '/' {
      let d = self.peek(1)
      if d == 47 {
        self.position = self.position + 2
        while self.position < length && !is_line_terminator(self.character()) {
          self.position = self.position + 1
        }
        continue
      }
      if d == 42 {
        let comment_start = self.position
        self.position = self.position + 2
        let mut closed = false
        while self.position < length {
          if self.source[self.position] == '*' && self.peek(1) == 47 {
            self.position = self.position + 2
            closed = true
            break
          }
          if is_line_terminator(self.character()) {
            self.newline_before = true
          }
          self.position = self.position + 1
        }
        guard closed else {
          raise Unterminated(position_at(self.source, comment_start), "comment")
        }
        continue
      }
    }
    break
  }
}

///|
fn Lexer::scan_identifier(self : Lexer) -> Unit raise ParseError {
  if self.character() == '#' {
    self.position += 1
  }
  let mut first = true
  while self.position < self.source.length() {
    if self.character() == '\\' {
      let (character, end) = identifier_escape(self.source, self.position)
      let code = character.to_int()
      let valid = code >= 0x80 ||
        code == 36 ||
        code == 95 ||
        (code >= 65 && code <= 90) ||
        (code >= 97 && code <= 122) ||
        (!first && code >= 48 && code <= 57)
      guard valid && !character.is_whitespace() else {
        raise self.invalid_escape("invalid identifier escape")
      }
      self.position = end
    } else if is_identifier_continue(self.character()) &&
      !is_whitespace(self.character()) &&
      !is_line_terminator(self.character()) {
      self.position += 1
    } else {
      break
    }
    first = false
  }
  guard !first else { raise self.syntax_error("identifier expected") }
  self.token = Identifier
  self.end = self.position
}

///|
fn Lexer::scan_digits(self : Lexer, is_valid : (UInt16) -> Bool) -> Unit {
  let length = self.source.length()
  while self.position < length {
    let c = self.character()
    if is_valid(c) || c == '_' {
      self.position = self.position + 1
    } else {
      break
    }
  }
}

///|
fn Lexer::scan_number(self : Lexer) -> Unit {
  let c = self.character()
  if c == '0' {
    let d = self.character_at(1)
    if d == 'x' || d == 'X' {
      self.position = self.position + 2
      self.scan_digits(is_hex)
      self.finish_number()
      return
    }
    if d == 'b' || d == 'B' {
      self.position = self.position + 2
      self.scan_digits(x => x == '0' || x == '1')
      self.finish_number()
      return
    }
    if d == 'o' || d == 'O' {
      self.position = self.position + 2
      self.scan_digits(is_oct)
      self.finish_number()
      return
    }
  }
  self.scan_digits(is_digit)
  if self.character() == '.' {
    self.position = self.position + 1
    self.scan_digits(is_digit)
  }
  let e = self.character()
  if e == 'e' || e == 'E' {
    self.position = self.position + 1
    let s = self.character()
    if s == '+' || s == '-' {
      self.position = self.position + 1
    }
    self.scan_digits(is_digit)
  }
  self.finish_number()
}

///|
fn Lexer::finish_number(self : Lexer) -> Unit {
  if self.character() == 'n' {
    self.position = self.position + 1
  }
  self.token = Number
  self.end = self.position
}

///|
fn Lexer::scan_string(self : Lexer, quote : UInt16) -> Unit raise ParseError {
  self.position += 1
  while self.position < self.source.length() {
    let c = self.character()
    if c == quote {
      self.position += 1
      self.token = StringLiteral
      self.end = self.position
      return
    }
    guard c != '\n' && c != '\r' else {
      raise self.syntax_error("newline in string literal")
    }
    if c == '\\' {
      guard self.position + 1 < self.source.length() else {
        raise self.unterminated("string escape")
      }
      self.position += 2
      if self.source[self.position - 1] == '\r' && self.character() == '\n' {
        self.position += 1
      }
    } else {
      self.position += 1
    }
  }
  raise self.unterminated("string literal")
}

///|
fn Lexer::scan_regex(self : Lexer) -> Unit raise ParseError {
  self.position += 1
  let mut in_class = false
  while self.position < self.source.length() {
    let c = self.character()
    guard !is_line_terminator(c) else {
      raise self.syntax_error("newline in regular expression")
    }
    if c == '\\' {
      guard self.position + 1 < self.source.length() &&
        !is_line_terminator(self.character_at(1)) else {
        raise self.unterminated("regular expression escape")
      }
      self.position += 2
      continue
    }
    if c == '/' && !in_class {
      self.position += 1
      while is_identifier_continue(self.character()) &&
            !is_whitespace(self.character()) &&
            !is_line_terminator(self.character()) {
        self.position += 1
      }
      self.token = Regex
      self.end = self.position
      return
    }
    if c == '[' {
      in_class = true
    }
    if c == ']' {
      in_class = false
    }
    self.position += 1
  }
  raise self.unterminated("regular expression")
}

///|
fn Lexer::scan_template_chunk(
  self : Lexer,
  is_start : Bool,
) -> Unit raise ParseError {
  let length = self.source.length()
  let begin = self.position
  while self.position < length {
    let c = self.source[self.position]
    if c == '\\' {
      guard self.position + 1 < length else {
        raise self.unterminated("template escape")
      }
      self.position += 2
      continue
    }
    if c == '`' {
      self.quasi_start = begin
      self.quasi_end = self.position
      self.position = self.position + 1
      self.token = if is_start { TemplateNoSubstitution } else { TemplateTail }
      self.end = self.position
      return
    }
    if c == '$' && self.peek(1) == 123 {
      self.quasi_start = begin
      self.quasi_end = self.position
      self.position = self.position + 2
      self.token = if is_start { TemplateHead } else { TemplateMiddle }
      self.end = self.position
      return
    }
    self.position = self.position + 1
  }
  raise self.unterminated("template literal")
}

///|
fn Lexer::rescan_template_continue(self : Lexer) -> Unit raise ParseError {
  self.start = self.position
  self.scan_template_chunk(false)
}

///|
fn Lexer::next(self : Lexer) -> Unit raise ParseError {
  self.newline_before = false
  self.skip_trivia()
  self.start = self.position
  let length = self.source.length()
  if self.position >= length {
    self.token = EndOfFile
    self.end = self.position
    return
  }
  let c = self.character()
  if is_identifier_start(c) || c == '\\' {
    self.scan_identifier()
    return
  }
  if is_digit(c) {
    self.scan_number()
    return
  }
  if c == '.' && is_digit(self.character_at(1)) {
    self.scan_number()
    return
  }
  if c == '\'' || c == '"' {
    self.scan_string(c)
    return
  }
  if c == '`' {
    self.position = self.position + 1
    self.scan_template_chunk(true)
    return
  }
  self.scan_punctuation()
}

///|
fn Lexer::scan_punctuation(self : Lexer) -> Unit raise ParseError {
  let (token, rest) : (Token, StringView) = lexmatch
    self.source.view(start_offset=self.position) with longest {
    (re"^\(", after=rest) => (LeftParenthesis, rest)
    (re"^\)", after=rest) => (RightParenthesis, rest)
    (re"^\[", after=rest) => (LeftBracket, rest)
    (re"^\]", after=rest) => (RightBracket, rest)
    (re"^[{]", after=rest) => (LeftBrace, rest)
    (re"^[}]", after=rest) => (RightBrace, rest)
    (re"^;", after=rest) => (Semicolon, rest)
    (re"^,", after=rest) => (Comma, rest)
    (re"^:", after=rest) => (Colon, rest)
    (re"^~", after=rest) => (Tilde, rest)
    (re"^\?", after=rest) => (Question, rest)
    (re"^\?\?", after=rest) => (NullishCoalescing, rest)
    (re"^\?\?=", after=rest) => (NullishCoalescingAssignment, rest)
    (re"^\.", after=rest) => (Dot, rest)
    (re"^\.\.\.", after=rest) => (Ellipsis, rest)
    (re"^=", after=rest) => (Assignment, rest)
    (re"^=>", after=rest) => (Arrow, rest)
    (re"^==", after=rest) => (Equal, rest)
    (re"^===", after=rest) => (StrictlyEqual, rest)
    (re"^!", after=rest) => (LogicalNot, rest)
    (re"^!=", after=rest) => (NotEqual, rest)
    (re"^!==", after=rest) => (StrictlyNotEqual, rest)
    (re"^<", after=rest) => (LessThan, rest)
    (re"^<=", after=rest) => (LessThanOrEqual, rest)
    (re"^<<", after=rest) => (ShiftLeft, rest)
    (re"^<<=", after=rest) => (LeftShiftAssignment, rest)
    (re"^>", after=rest) => (GreaterThan, rest)
    (re"^>=", after=rest) => (GreaterThanOrEqual, rest)
    (re"^>>", after=rest) => (ShiftRight, rest)
    (re"^>>=", after=rest) => (RightShiftAssignment, rest)
    (re"^>>>", after=rest) => (UnsignedShiftRight, rest)
    (re"^>>>=", after=rest) => (UnsignedRightShiftAssignment, rest)
    (re"^&", after=rest) => (BitwiseAnd, rest)
    (re"^&=", after=rest) => (BitwiseAndAssignment, rest)
    (re"^&&", after=rest) => (LogicalAnd, rest)
    (re"^&&=", after=rest) => (LogicalAndAssignment, rest)
    (re"^\|", after=rest) => (BitwiseOr, rest)
    (re"^\|=", after=rest) => (BitwiseOrAssignment, rest)
    (re"^\|\|", after=rest) => (LogicalOr, rest)
    (re"^\|\|=", after=rest) => (LogicalOrAssignment, rest)
    (re"^\^", after=rest) => (BitwiseXor, rest)
    (re"^\^=", after=rest) => (BitwiseXorAssignment, rest)
    (re"^\+", after=rest) => (Plus, rest)
    (re"^\+\+", after=rest) => (Increment, rest)
    (re"^\+=", after=rest) => (AdditionAssignment, rest)
    (re"^-", after=rest) => (Minus, rest)
    (re"^--", after=rest) => (Decrement, rest)
    (re"^-=", after=rest) => (SubtractionAssignment, rest)
    (re"^\*", after=rest) => (Asterisk, rest)
    (re"^\*\*", after=rest) => (ExponentiationOperator, rest)
    (re"^\*=", after=rest) => (MultiplicationAssignment, rest)
    (re"^\*\*=", after=rest) => (ExponentiationAssignment, rest)
    (re"^%", after=rest) => (Percent, rest)
    (re"^%=", after=rest) => (ModuloAssignment, rest)
    (re"^/", after=rest) => (Slash, rest)
    (re"^/=", after=rest) => (DivisionAssignment, rest)
    (re"^\?\.", after=rest) =>
      // Before a digit, ? starts a conditional and the dot belongs to a number.
      if is_digit(self.character_at(2)) {
        (Question, self.source.view(start_offset=self.position + 1))
      } else {
        (OptionalChaining, rest)
      }
    _ =>
      raise UnexpectedCharacter(
        position_at(self.source, self.position),
        self.character().to_int().to_char().unwrap_or('?'),
      )
  }
  self.token = token
  self.position = self.source.length() - rest.length()
  self.end = self.position
}

///|
fn identifier_escape(
  source : String,
  start : Int,
) -> (Char, Int) raise ParseError {
  guard start + 2 < source.length() && source[start + 1] == 'u' else {
    raise InvalidEscape(
      position_at(source, start),
      "expected Unicode identifier escape",
    )
  }
  let braced = source[start + 2] == '{'
  let begin = start + (if braced { 3 } else { 2 })
  let (value, end) = for value = 0, position = begin {
    if braced && position < source.length() && source[position] == '}' {
      guard position > begin else {
        raise InvalidEscape(
          position_at(source, position),
          "empty Unicode escape",
        )
      }
      break (value, position + 1)
    }
    if !braced && position == begin + 4 {
      break (value, position)
    }
    guard position < source.length() &&
      is_hex(source[position]) &&
      value <= 0x10FFFF / 16 else {
      raise InvalidEscape(
        position_at(source, position),
        "invalid Unicode escape",
      )
    }
    let c = source[position].to_int()
    let digit = if c <= 57 { c - 48 } else if c <= 70 { c - 55 } else { c - 87 }
    continue value * 16 + digit, position + 1
  }
  guard value.to_char() is Some(character) else {
    raise InvalidEscape(
      position_at(source, start),
      "invalid Unicode code point",
    )
  }
  (character, end)
}

///|
fn identifier_name(
  source : String,
  start : Int,
  end : Int,
) -> String raise ParseError {
  let output = StringBuilder()
  let mut position = start
  while position < end {
    if source[position] == '\\' {
      let (character, next) = identifier_escape(source, position)
      output.write_char(character)
      position = next
    } else {
      let begin = position
      while position < end && source[position] != '\\' {
        position += 1
      }
      output.write_substring(source, begin, position - begin)
    }
  }
  output.to_string()
}
