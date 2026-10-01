///|
/// Raw names keep the lexer's permissive non-ASCII rule, excluding trivia.
/// Escaped characters are decoded and validated separately.
const RAW_IDENTIFIER_RUN : Regex = re"^([A-Za-z0-9_$#]|[^\u0000-\u007F\u0085\u00A0\u1680\u2000-\u200A\u2028\u2029\u202F\u205F\u3000\uFEFF])+"

///|
fn raw_identifier_end(source : String, start : Int) -> Int {
  for position = start {
    let end = lexmatch source.view(start_offset=position) with longest {
      (RAW_IDENTIFIER_RUN, after=rest) => rest.start_offset()
      _ => position
    }
    // Regex matches Unicode scalars; raw identifiers also accept lone surrogates.
    guard end < source.length() && is_surrogate_code_unit(source[end]) else {
      break end
    }
    continue end + 1
  }
}

///|
fn is_surrogate_code_unit(character : UInt16) -> Bool {
  character >= 0xD800 && character <= 0xDFFF
}

///|
fn Lexer::identifier(self : Lexer) -> Unit raise ParseError {
  if self.character() == '#' {
    self.position += 1
  }
  let name_start = self.position
  while self.position < self.source.length() {
    self.position = raw_identifier_end(self.source, self.position)
    guard self.character() == '\\' else { break }
    let (character, end) = identifier_escape(self.source, self.position)
    guard is_escaped_identifier_character(
      character,
      self.position == name_start,
    ) else {
      raise self.invalid_escape("invalid identifier escape")
    }
    self.position = end
  }
  guard self.position > name_start else {
    raise self.syntax_error("identifier expected")
  }
  self.token = Identifier
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
  let (code_point, end) = if source[start + 2] == '{' {
    read_braced_identifier_escape(source, start + 3)
  } else {
    read_fixed_identifier_escape(source, start + 2)
  }
  guard code_point.to_char() is Some(character) else {
    raise InvalidEscape(
      position_at(source, start),
      "invalid Unicode code point",
    )
  }
  (character, end)
}

///|
fn read_fixed_identifier_escape(
  source : String,
  digits_start : Int,
) -> (Int, Int) raise ParseError {
  let digits_end = digits_start + 4
  for position = digits_start, code_point = 0 {
    guard position < digits_end else { break (code_point, digits_end) }
    let decoded = append_unicode_escape_digit(source, position, code_point)
    continue position + 1, decoded
  }
}

///|
fn read_braced_identifier_escape(
  source : String,
  digits_start : Int,
) -> (Int, Int) raise ParseError {
  for position = digits_start, code_point = 0 {
    if position < source.length() && source[position] == '}' {
      guard position > digits_start else {
        raise InvalidEscape(
          position_at(source, position),
          "empty Unicode escape",
        )
      }
      break (code_point, position + 1)
    }
    let decoded = append_unicode_escape_digit(source, position, code_point)
    continue position + 1, decoded
  }
}

///|
fn append_unicode_escape_digit(
  source : String,
  position : Int,
  code_point : Int,
) -> Int raise ParseError {
  guard position < source.length() && code_point <= 0x10FFFF / 16 else {
    raise InvalidEscape(position_at(source, position), "invalid Unicode escape")
  }
  guard hex_digit_value(source[position]) is Some(digit) else {
    raise InvalidEscape(position_at(source, position), "invalid Unicode escape")
  }
  code_point * 16 + digit
}

///|
fn identifier_name(
  source : String,
  start : Int,
  end : Int,
) -> String raise ParseError {
  let output = StringBuilder()
  for position = start {
    guard position < end else { break }
    if source[position] == '\\' {
      let (character, next) = identifier_escape(source, position)
      output.write_char(character)
      continue next
    } else {
      let literal_end = identifier_literal_end(source, position, end)
      output.write_substring(source, position, literal_end - position)
      continue literal_end
    }
  }
  output.to_string()
}

///|
fn identifier_literal_end(source : String, start : Int, end : Int) -> Int {
  for position = start {
    let literal_end = lexmatch
      source.view(start_offset=position, end_offset=end) with longest {
      (re"^[^\\]+", after=rest) => rest.start_offset()
      _ => position
    }
    guard literal_end < end && is_surrogate_code_unit(source[literal_end]) else {
      break literal_end
    }
    continue literal_end + 1
  }
}
