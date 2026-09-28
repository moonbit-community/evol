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
  if self.token != Identifier {
    return false
  }
  let n = keyword.length()
  if self.end - self.start != n {
    return false
  }
  let mut i = 0
  while i < n {
    if self.source[self.start + i] != keyword[i] {
      return false
    }
    i = i + 1
  }
  true
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
  if c == '/' {
    let d = self.peek(1)
    if d == 61 {
      self.position = self.position + 2
      self.token = DivisionAssignment
      self.end = self.position
      return
    }
    self.position = self.position + 1
    self.token = Slash
    self.end = self.position
    return
  }
  self.scan_punctuation(c.to_int())
}

///|
fn Lexer::scan_punctuation(self : Lexer, c : Int) -> Unit raise ParseError {
  match c {
    40 => {
      self.position = self.position + 1
      self.token = LeftParenthesis
    }
    41 => {
      self.position = self.position + 1
      self.token = RightParenthesis
    }
    91 => {
      self.position = self.position + 1
      self.token = LeftBracket
    }
    93 => {
      self.position = self.position + 1
      self.token = RightBracket
    }
    123 => {
      self.position = self.position + 1
      self.token = LeftBrace
    }
    125 => {
      self.position = self.position + 1
      self.token = RightBrace
    }
    59 => {
      self.position = self.position + 1
      self.token = Semicolon
    }
    44 => {
      self.position = self.position + 1
      self.token = Comma
    }
    58 => {
      self.position = self.position + 1
      self.token = Colon
    }
    126 => {
      self.position = self.position + 1
      self.token = Tilde
    }
    63 =>
      if self.peek(1) == 46 && !is_digit(self.character_at(2)) {
        self.position = self.position + 2
        self.token = OptionalChaining
      } else if self.peek(1) == 63 && self.peek(2) == 61 {
        self.position = self.position + 3
        self.token = NullishCoalescingAssignment
      } else if self.peek(1) == 63 {
        self.position = self.position + 2
        self.token = NullishCoalescing
      } else {
        self.position = self.position + 1
        self.token = Question
      }
    46 =>
      if self.peek(1) == 46 && self.peek(2) == 46 {
        self.position = self.position + 3
        self.token = Ellipsis
      } else {
        self.position = self.position + 1
        self.token = Dot
      }
    61 =>
      if self.peek(1) == 62 {
        self.position = self.position + 2
        self.token = Arrow
      } else if self.peek(1) == 61 && self.peek(2) == 61 {
        self.position = self.position + 3
        self.token = StrictlyEqual
      } else if self.peek(1) == 61 {
        self.position = self.position + 2
        self.token = Equal
      } else {
        self.position = self.position + 1
        self.token = Assignment
      }
    33 =>
      if self.peek(1) == 61 && self.peek(2) == 61 {
        self.position = self.position + 3
        self.token = StrictlyNotEqual
      } else if self.peek(1) == 61 {
        self.position = self.position + 2
        self.token = NotEqual
      } else {
        self.position = self.position + 1
        self.token = LogicalNot
      }
    60 =>
      if self.peek(1) == 60 && self.peek(2) == 61 {
        self.position = self.position + 3
        self.token = LeftShiftAssignment
      } else if self.peek(1) == 60 {
        self.position = self.position + 2
        self.token = ShiftLeft
      } else if self.peek(1) == 61 {
        self.position = self.position + 2
        self.token = LessThanOrEqual
      } else {
        self.position = self.position + 1
        self.token = LessThan
      }
    62 =>
      if self.peek(1) == 62 && self.peek(2) == 62 && self.peek(3) == 61 {
        self.position = self.position + 4
        self.token = UnsignedRightShiftAssignment
      } else if self.peek(1) == 62 && self.peek(2) == 62 {
        self.position = self.position + 3
        self.token = UnsignedShiftRight
      } else if self.peek(1) == 62 && self.peek(2) == 61 {
        self.position = self.position + 3
        self.token = RightShiftAssignment
      } else if self.peek(1) == 62 {
        self.position = self.position + 2
        self.token = ShiftRight
      } else if self.peek(1) == 61 {
        self.position = self.position + 2
        self.token = GreaterThanOrEqual
      } else {
        self.position = self.position + 1
        self.token = GreaterThan
      }
    38 =>
      if self.peek(1) == 38 && self.peek(2) == 61 {
        self.position = self.position + 3
        self.token = LogicalAndAssignment
      } else if self.peek(1) == 38 {
        self.position = self.position + 2
        self.token = LogicalAnd
      } else if self.peek(1) == 61 {
        self.position = self.position + 2
        self.token = BitwiseAndAssignment
      } else {
        self.position = self.position + 1
        self.token = BitwiseAnd
      }
    124 =>
      if self.peek(1) == 124 && self.peek(2) == 61 {
        self.position = self.position + 3
        self.token = LogicalOrAssignment
      } else if self.peek(1) == 124 {
        self.position = self.position + 2
        self.token = LogicalOr
      } else if self.peek(1) == 61 {
        self.position = self.position + 2
        self.token = BitwiseOrAssignment
      } else {
        self.position = self.position + 1
        self.token = BitwiseOr
      }
    94 =>
      if self.peek(1) == 61 {
        self.position = self.position + 2
        self.token = BitwiseXorAssignment
      } else {
        self.position = self.position + 1
        self.token = BitwiseXor
      }
    43 =>
      if self.peek(1) == 43 {
        self.position = self.position + 2
        self.token = Increment
      } else if self.peek(1) == 61 {
        self.position = self.position + 2
        self.token = AdditionAssignment
      } else {
        self.position = self.position + 1
        self.token = Plus
      }
    45 =>
      if self.peek(1) == 45 {
        self.position = self.position + 2
        self.token = Decrement
      } else if self.peek(1) == 61 {
        self.position = self.position + 2
        self.token = SubtractionAssignment
      } else {
        self.position = self.position + 1
        self.token = Minus
      }
    42 =>
      if self.peek(1) == 42 && self.peek(2) == 61 {
        self.position = self.position + 3
        self.token = ExponentiationAssignment
      } else if self.peek(1) == 42 {
        self.position = self.position + 2
        self.token = ExponentiationOperator
      } else if self.peek(1) == 61 {
        self.position = self.position + 2
        self.token = MultiplicationAssignment
      } else {
        self.position = self.position + 1
        self.token = Asterisk
      }
    37 =>
      if self.peek(1) == 61 {
        self.position = self.position + 2
        self.token = ModuloAssignment
      } else {
        self.position = self.position + 1
        self.token = Percent
      }
    _ =>
      raise UnexpectedCharacter(
        position_at(self.source, self.position),
        c.to_char().unwrap_or('?'),
      )
  }
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
