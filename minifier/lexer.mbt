///|
priv struct Lexer {
  source : String
  // The next unread UTF-16 code unit; token spans are [start, end).
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
fn Lexer::is_keyword(self : Lexer, keyword : String) -> Bool {
  self.token == Identifier &&
  self.source
  .view(start_offset=self.start, end_offset=self.end)
  .equal_to_string(keyword)
}

///|
const LINE_TERMINATORS : Regex = re"[\n\r\u2028\u2029]+"

///|
// Keep the existing whitespace set; line terminators are tracked separately.
const TOKEN_WHITESPACE : Regex = re"[ \t\v\f\u0085\u00A0\u1680\u2000-\u200A\u202F\u205F\u3000\uFEFF]+"

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
fn Lexer::skip_trivia(self : Lexer) -> Unit raise ParseError {
  while true {
    lexmatch self.source.view(start_offset=self.position) with longest {
      (re"^" + LINE_TERMINATORS, after=rest) => {
        self.newline_before = true
        self.position = rest.start_offset()
      }
      (re"^" + TOKEN_WHITESPACE, after=rest) =>
        self.position = rest.start_offset()
      (re"^//[^\n\r\u2028\u2029]*", after=rest) => {
        self.position = rest.start_offset()
        self.line_comment_tail()
      }
      (re"^#![^\n\r\u2028\u2029]*", after=rest) => {
        guard self.position == 0 else { return }
        self.position = rest.start_offset()
        self.line_comment_tail()
      }
      (re"^/\*", after=rest) => {
        let comment_start = self.position
        self.position = rest.start_offset()
        self.block_comment_tail(comment_start)
      }
      _ => return
    }
  }
}

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
fn Lexer::next(self : Lexer) -> Unit raise ParseError {
  self.newline_before = false
  self.skip_trivia()
  self.start = self.position
  lexmatch self.source.view(start_offset=self.position) with longest {
    (re"^" + NUMBER_SPELLING, after=rest) => self.finish_token(Number, rest)
    (re"^(?:[A-Za-z_$#\\]|[^\u0000-\u007F])", after=_) => self.identifier()
    (re"^'", after=_) => self.string('\'')
    (re"^\"", after=_) => self.string('"')
    (re"^`", after=rest) => {
      self.position = rest.start_offset()
      self.template_chunk(true)
    }
    (re"^$", after=rest) => self.finish_token(EndOfFile, rest)
    _ =>
      if self.source.get_char(self.position) is None {
        self.identifier()
      } else {
        self.punctuation()
      }
  }
}

///|
fn Lexer::punctuation(self : Lexer) -> Unit raise ParseError {
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
    // Before a digit, ? starts a conditional and the dot belongs to a number.
    (re"^\?\.[0-9]", after=_) =>
      (Question, self.source.view(start_offset=self.position + 1))
    (re"^\?\.", after=rest) => (OptionalChaining, rest)
    _ =>
      raise UnexpectedCharacter(
        position_at(self.source, self.position),
        self.source[self.position].to_int().to_char().unwrap_or('?'),
      )
  }
  self.finish_token(token, rest)
}
