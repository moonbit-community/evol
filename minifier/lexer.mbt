///|
fn is_line_term(c : UInt16) -> Bool {
  c == '\n' || c == '\r' || c == 0x2028 || c == 0x2029
}

///|
fn is_ws(c : UInt16) -> Bool {
  if c < 0x80 {
    c == ' ' || c == '\t' || c == 0x0B || c == 0x0C
  } else {
    c == 0xFEFF || c.to_int().to_char().map_or(false, ch => ch.is_whitespace())
  }
}

///|
fn is_id_start(c : UInt16) -> Bool {
  c == '$' ||
  c == '_' ||
  c == '#' ||
  (c >= 'a' && c <= 'z') ||
  (c >= 'A' && c <= 'Z') ||
  c >= 0x80
}

///|
fn is_id_continue(c : UInt16) -> Bool {
  is_id_start(c) || (c >= '0' && c <= '9')
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
  src : String
  mut pos : Int
  mut tok : Tok
  mut start : Int
  mut end : Int
  mut newline_before : Bool
  mut quasi_start : Int
  mut quasi_end : Int
}

///|
fn Lexer::new(src : String) -> Lexer {
  {
    src,
    pos: 0,
    tok: Eof,
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
  pos : Int
  tok : Tok
  start : Int
  end : Int
  newline_before : Bool
  quasi_start : Int
  quasi_end : Int
}

///|
fn Lexer::snapshot(self : Lexer) -> LexerSnapshot {
  {
    pos: self.pos,
    tok: self.tok,
    start: self.start,
    end: self.end,
    newline_before: self.newline_before,
    quasi_start: self.quasi_start,
    quasi_end: self.quasi_end,
  }
}

///|
fn Lexer::restore(self : Lexer, other : LexerSnapshot) -> Unit {
  self.pos = other.pos
  self.tok = other.tok
  self.start = other.start
  self.end = other.end
  self.newline_before = other.newline_before
  self.quasi_start = other.quasi_start
  self.quasi_end = other.quasi_end
}

///|
fn Lexer::ch(self : Lexer) -> UInt16 {
  self.ch_at(0)
}

///|
fn Lexer::ch_at(self : Lexer, off : Int) -> UInt16 {
  let i = self.pos + off
  if i < 0 || i >= self.src.length() {
    0
  } else {
    self.src[i]
  }
}

///|
fn Lexer::peek(self : Lexer, off : Int) -> Int {
  let i = self.pos + off
  if i < 0 || i >= self.src.length() {
    -1
  } else {
    self.src[i].to_int()
  }
}

///|
fn Lexer::is_kw(self : Lexer, kw : String) -> Bool {
  if self.tok != Ident {
    return false
  }
  let n = kw.length()
  if self.end - self.start != n {
    return false
  }
  let mut i = 0
  while i < n {
    if self.src[self.start + i] != kw[i] {
      return false
    }
    i = i + 1
  }
  true
}

///|
fn Lexer::skip_trivia(self : Lexer) -> Unit raise ParseError {
  let len = self.src.length()
  if self.pos == 0 && len >= 2 && self.src[0] == '#' && self.src[1] == '!' {
    while self.pos < len && !is_line_term(self.ch()) {
      self.pos = self.pos + 1
    }
  }
  while self.pos < len {
    let c = self.ch()
    if is_line_term(c) {
      self.newline_before = true
      self.pos = self.pos + 1
      continue
    }
    if is_ws(c) {
      self.pos = self.pos + 1
      continue
    }
    if c == '/' {
      let d = self.peek(1)
      if d == 47 {
        self.pos = self.pos + 2
        while self.pos < len && !is_line_term(self.ch()) {
          self.pos = self.pos + 1
        }
        continue
      }
      if d == 42 {
        self.pos = self.pos + 2
        let mut closed = false
        while self.pos < len {
          if self.src[self.pos] == '*' && self.peek(1) == 47 {
            self.pos = self.pos + 2
            closed = true
            break
          }
          if is_line_term(self.ch()) {
            self.newline_before = true
          }
          self.pos = self.pos + 1
        }
        guard closed else { raise Unexpected("unterminated comment") }
        continue
      }
    }
    break
  }
}

///|
fn Lexer::scan_ident(self : Lexer) -> Unit raise ParseError {
  if self.ch() == '#' {
    self.pos += 1
  }
  let mut first = true
  while self.pos < self.src.length() {
    if self.ch() == '\\' {
      let (ch, end) = identifier_escape(self.src, self.pos)
      let code = ch.to_int()
      let valid = code >= 0x80 ||
        code == 36 ||
        code == 95 ||
        (code >= 65 && code <= 90) ||
        (code >= 97 && code <= 122) ||
        (!first && code >= 48 && code <= 57)
      guard valid && !ch.is_whitespace() else {
        raise Unexpected("invalid identifier escape")
      }
      self.pos = end
    } else if is_id_continue(self.ch()) &&
      !is_ws(self.ch()) &&
      !is_line_term(self.ch()) {
      self.pos += 1
    } else {
      break
    }
    first = false
  }
  guard !first else { raise Unexpected("identifier expected") }
  self.tok = Ident
  self.end = self.pos
}

///|
fn Lexer::scan_digits(self : Lexer, is_valid : (UInt16) -> Bool) -> Unit {
  let len = self.src.length()
  while self.pos < len {
    let c = self.ch()
    if is_valid(c) || c == '_' {
      self.pos = self.pos + 1
    } else {
      break
    }
  }
}

///|
fn Lexer::scan_number(self : Lexer) -> Unit {
  let c = self.ch()
  if c == '0' {
    let d = self.ch_at(1)
    if d == 'x' || d == 'X' {
      self.pos = self.pos + 2
      self.scan_digits(is_hex)
      self.finish_number()
      return
    }
    if d == 'b' || d == 'B' {
      self.pos = self.pos + 2
      self.scan_digits(x => x == '0' || x == '1')
      self.finish_number()
      return
    }
    if d == 'o' || d == 'O' {
      self.pos = self.pos + 2
      self.scan_digits(is_oct)
      self.finish_number()
      return
    }
  }
  self.scan_digits(is_digit)
  if self.ch() == '.' {
    self.pos = self.pos + 1
    self.scan_digits(is_digit)
  }
  let e = self.ch()
  if e == 'e' || e == 'E' {
    self.pos = self.pos + 1
    let s = self.ch()
    if s == '+' || s == '-' {
      self.pos = self.pos + 1
    }
    self.scan_digits(is_digit)
  }
  self.finish_number()
}

///|
fn Lexer::finish_number(self : Lexer) -> Unit {
  if self.ch() == 'n' {
    self.pos = self.pos + 1
  }
  self.tok = Number
  self.end = self.pos
}

///|
fn Lexer::scan_string(self : Lexer, quote : UInt16) -> Unit raise ParseError {
  self.pos += 1
  while self.pos < self.src.length() {
    let c = self.ch()
    if c == quote {
      self.pos += 1
      self.tok = Str
      self.end = self.pos
      return
    }
    guard c != '\n' && c != '\r' else {
      raise Unexpected("newline in string literal")
    }
    if c == '\\' {
      guard self.pos + 1 < self.src.length() else {
        raise Unexpected("unterminated string escape")
      }
      self.pos += 2
      if self.src[self.pos - 1] == '\r' && self.ch() == '\n' {
        self.pos += 1
      }
    } else {
      self.pos += 1
    }
  }
  raise Unexpected("unterminated string literal")
}

///|
fn Lexer::scan_regex(self : Lexer) -> Unit raise ParseError {
  self.pos += 1
  let mut in_class = false
  while self.pos < self.src.length() {
    let c = self.ch()
    guard !is_line_term(c) else {
      raise Unexpected("newline in regular expression")
    }
    if c == '\\' {
      guard self.pos + 1 < self.src.length() && !is_line_term(self.ch_at(1)) else {
        raise Unexpected("unterminated regular expression escape")
      }
      self.pos += 2
      continue
    }
    if c == '/' && !in_class {
      self.pos += 1
      while is_id_continue(self.ch()) &&
            !is_ws(self.ch()) &&
            !is_line_term(self.ch()) {
        self.pos += 1
      }
      self.tok = Regex
      self.end = self.pos
      return
    }
    if c == '[' {
      in_class = true
    }
    if c == ']' {
      in_class = false
    }
    self.pos += 1
  }
  raise Unexpected("unterminated regular expression")
}

///|
fn Lexer::scan_template_chunk(
  self : Lexer,
  is_start : Bool,
) -> Unit raise ParseError {
  let len = self.src.length()
  let begin = self.pos
  while self.pos < len {
    let c = self.src[self.pos]
    if c == '\\' {
      guard self.pos + 1 < len else {
        raise Unexpected("unterminated template escape")
      }
      self.pos += 2
      continue
    }
    if c == '`' {
      self.quasi_start = begin
      self.quasi_end = self.pos
      self.pos = self.pos + 1
      self.tok = if is_start { TmplNoSub } else { TmplTail }
      self.end = self.pos
      return
    }
    if c == '$' && self.peek(1) == 123 {
      self.quasi_start = begin
      self.quasi_end = self.pos
      self.pos = self.pos + 2
      self.tok = if is_start { TmplHead } else { TmplMiddle }
      self.end = self.pos
      return
    }
    self.pos = self.pos + 1
  }
  raise Unexpected("unterminated template literal")
}

///|
fn Lexer::rescan_template_continue(self : Lexer) -> Unit raise ParseError {
  self.start = self.pos
  self.scan_template_chunk(false)
}

///|
fn Lexer::next(self : Lexer) -> Unit raise ParseError {
  self.newline_before = false
  self.skip_trivia()
  self.start = self.pos
  let len = self.src.length()
  if self.pos >= len {
    self.tok = Eof
    self.end = self.pos
    return
  }
  let c = self.ch()
  if is_id_start(c) || c == '\\' {
    self.scan_ident()
    return
  }
  if is_digit(c) {
    self.scan_number()
    return
  }
  if c == '.' && is_digit(self.ch_at(1)) {
    self.scan_number()
    return
  }
  if c == '\'' || c == '"' {
    self.scan_string(c)
    return
  }
  if c == '`' {
    self.pos = self.pos + 1
    self.scan_template_chunk(true)
    return
  }
  if c == '/' {
    let d = self.peek(1)
    if d == 61 {
      self.pos = self.pos + 2
      self.tok = DivAssign
      self.end = self.pos
      return
    }
    self.pos = self.pos + 1
    self.tok = Slash
    self.end = self.pos
    return
  }
  self.scan_punct(c.to_int())
}

///|
fn Lexer::scan_punct(self : Lexer, c : Int) -> Unit raise ParseError {
  match c {
    40 => {
      self.pos = self.pos + 1
      self.tok = LParen
    }
    41 => {
      self.pos = self.pos + 1
      self.tok = RParen
    }
    91 => {
      self.pos = self.pos + 1
      self.tok = LBracket
    }
    93 => {
      self.pos = self.pos + 1
      self.tok = RBracket
    }
    123 => {
      self.pos = self.pos + 1
      self.tok = LBrace
    }
    125 => {
      self.pos = self.pos + 1
      self.tok = RBrace
    }
    59 => {
      self.pos = self.pos + 1
      self.tok = Semicolon
    }
    44 => {
      self.pos = self.pos + 1
      self.tok = Comma
    }
    58 => {
      self.pos = self.pos + 1
      self.tok = Colon
    }
    126 => {
      self.pos = self.pos + 1
      self.tok = Tilde
    }
    63 =>
      if self.peek(1) == 46 && !is_digit(self.ch_at(2)) {
        self.pos = self.pos + 2
        self.tok = QuestionDot
      } else if self.peek(1) == 63 && self.peek(2) == 61 {
        self.pos = self.pos + 3
        self.tok = NullishAssign
      } else if self.peek(1) == 63 {
        self.pos = self.pos + 2
        self.tok = Nullish
      } else {
        self.pos = self.pos + 1
        self.tok = Question
      }
    46 =>
      if self.peek(1) == 46 && self.peek(2) == 46 {
        self.pos = self.pos + 3
        self.tok = Ellipsis
      } else {
        self.pos = self.pos + 1
        self.tok = Dot
      }
    61 =>
      if self.peek(1) == 62 {
        self.pos = self.pos + 2
        self.tok = Arrow
      } else if self.peek(1) == 61 && self.peek(2) == 61 {
        self.pos = self.pos + 3
        self.tok = StrictEq
      } else if self.peek(1) == 61 {
        self.pos = self.pos + 2
        self.tok = Eq
      } else {
        self.pos = self.pos + 1
        self.tok = Assign
      }
    33 =>
      if self.peek(1) == 61 && self.peek(2) == 61 {
        self.pos = self.pos + 3
        self.tok = StrictNe
      } else if self.peek(1) == 61 {
        self.pos = self.pos + 2
        self.tok = Ne
      } else {
        self.pos = self.pos + 1
        self.tok = Not
      }
    60 =>
      if self.peek(1) == 60 && self.peek(2) == 61 {
        self.pos = self.pos + 3
        self.tok = ShlAssign
      } else if self.peek(1) == 60 {
        self.pos = self.pos + 2
        self.tok = Shl
      } else if self.peek(1) == 61 {
        self.pos = self.pos + 2
        self.tok = Le
      } else {
        self.pos = self.pos + 1
        self.tok = Lt
      }
    62 =>
      if self.peek(1) == 62 && self.peek(2) == 62 && self.peek(3) == 61 {
        self.pos = self.pos + 4
        self.tok = UshrAssign
      } else if self.peek(1) == 62 && self.peek(2) == 62 {
        self.pos = self.pos + 3
        self.tok = Ushr
      } else if self.peek(1) == 62 && self.peek(2) == 61 {
        self.pos = self.pos + 3
        self.tok = ShrAssign
      } else if self.peek(1) == 62 {
        self.pos = self.pos + 2
        self.tok = Shr
      } else if self.peek(1) == 61 {
        self.pos = self.pos + 2
        self.tok = Ge
      } else {
        self.pos = self.pos + 1
        self.tok = Gt
      }
    38 =>
      if self.peek(1) == 38 && self.peek(2) == 61 {
        self.pos = self.pos + 3
        self.tok = LogicalAndAssign
      } else if self.peek(1) == 38 {
        self.pos = self.pos + 2
        self.tok = LogicalAnd
      } else if self.peek(1) == 61 {
        self.pos = self.pos + 2
        self.tok = BitAndAssign
      } else {
        self.pos = self.pos + 1
        self.tok = BitAnd
      }
    124 =>
      if self.peek(1) == 124 && self.peek(2) == 61 {
        self.pos = self.pos + 3
        self.tok = LogicalOrAssign
      } else if self.peek(1) == 124 {
        self.pos = self.pos + 2
        self.tok = LogicalOr
      } else if self.peek(1) == 61 {
        self.pos = self.pos + 2
        self.tok = BitOrAssign
      } else {
        self.pos = self.pos + 1
        self.tok = BitOr
      }
    94 =>
      if self.peek(1) == 61 {
        self.pos = self.pos + 2
        self.tok = BitXorAssign
      } else {
        self.pos = self.pos + 1
        self.tok = BitXor
      }
    43 =>
      if self.peek(1) == 43 {
        self.pos = self.pos + 2
        self.tok = PlusPlus
      } else if self.peek(1) == 61 {
        self.pos = self.pos + 2
        self.tok = AddAssign
      } else {
        self.pos = self.pos + 1
        self.tok = Plus
      }
    45 =>
      if self.peek(1) == 45 {
        self.pos = self.pos + 2
        self.tok = MinusMinus
      } else if self.peek(1) == 61 {
        self.pos = self.pos + 2
        self.tok = SubAssign
      } else {
        self.pos = self.pos + 1
        self.tok = Minus
      }
    42 =>
      if self.peek(1) == 42 && self.peek(2) == 61 {
        self.pos = self.pos + 3
        self.tok = ExpAssign
      } else if self.peek(1) == 42 {
        self.pos = self.pos + 2
        self.tok = StarStar
      } else if self.peek(1) == 61 {
        self.pos = self.pos + 2
        self.tok = MulAssign
      } else {
        self.pos = self.pos + 1
        self.tok = Star
      }
    37 =>
      if self.peek(1) == 61 {
        self.pos = self.pos + 2
        self.tok = ModAssign
      } else {
        self.pos = self.pos + 1
        self.tok = Percent
      }
    _ => raise Unexpected("unexpected character at offset \{self.pos}")
  }
  self.end = self.pos
}

///|
fn identifier_escape(src : String, start : Int) -> (Char, Int) raise ParseError {
  guard start + 2 < src.length() && src[start + 1] == 'u' else {
    raise Unexpected("expected Unicode identifier escape")
  }
  let braced = src[start + 2] == '{'
  let begin = start + (if braced { 3 } else { 2 })
  let (value, end) = for value = 0, pos = begin {
    if braced && pos < src.length() && src[pos] == '}' {
      guard pos > begin else { raise Unexpected("empty Unicode escape") }
      break (value, pos + 1)
    }
    if !braced && pos == begin + 4 {
      break (value, pos)
    }
    guard pos < src.length() && is_hex(src[pos]) && value <= 0x10FFFF / 16 else {
      raise Unexpected("invalid Unicode escape")
    }
    let c = src[pos].to_int()
    let digit = if c <= 57 { c - 48 } else if c <= 70 { c - 55 } else { c - 87 }
    continue value * 16 + digit, pos + 1
  }
  guard value.to_char() is Some(ch) else {
    raise Unexpected("invalid Unicode code point")
  }
  (ch, end)
}

///|
fn identifier_name(
  src : String,
  start : Int,
  end : Int,
) -> String raise ParseError {
  let out = StringBuilder()
  let mut pos = start
  while pos < end {
    if src[pos] == '\\' {
      let (ch, next) = identifier_escape(src, pos)
      out.write_char(ch)
      pos = next
    } else {
      let begin = pos
      while pos < end && src[pos] != '\\' {
        pos += 1
      }
      out.write_substring(src, begin, pos - begin)
    }
  }
  out.to_string()
}
