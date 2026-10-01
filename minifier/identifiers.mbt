///|
/// Raw names retain the existing permissive non-ASCII character set.
const RAW_IDENTIFIER_START : Regex = re"(?:[A-Za-z_$]|[^\u0000-\u007F\u0085\u00A0\u1680\u2000-\u200A\u2028\u2029\u202F\u205F\u3000\uFEFF])"

///|
const RAW_IDENTIFIER_CONTINUATION : Regex = re"(?:[A-Za-z0-9_$#]|[^\u0000-\u007F\u0085\u00A0\u1680\u2000-\u200A\u2028\u2029\u202F\u205F\u3000\uFEFF])+"

///|
fn raw_identifier_end(source : String, start : Int) -> Int {
  for position = start {
    let end = lexmatch source.view(start_offset=position) with longest {
      (re"^" + RAW_IDENTIFIER_CONTINUATION, after=rest) => rest.start_offset()
      _ => position
    }
    // Regex matches Unicode scalars; raw identifiers also accept lone surrogates.
    guard end < source.length() && source.get_char(end) is None else {
      break end
    }
    continue end + 1
  }
}

///|
/// Continue a name only when escapes or ill-formed UTF-16 interrupt a raw run.
fn Lexer::identifier_tail(
  self : Lexer,
  name_start : Int,
) -> Unit raise ParseError {
  while self.position < self.source.length() {
    self.position = raw_identifier_end(self.source, self.position)
    lexmatch self.source.view(start_offset=self.position) {
      (re"^\\", after=_) => {
        let (character, end) = identifier_escape(self.source, self.position)
        let allowed = lexmatch character.to_string() {
          re"^[A-Za-z_$]$" => true
          re"^[0-9]$" => self.position != name_start
          // Unlike a raw name, an escaped name keeps U+FEFF as a character.
          re"^[^\u0000-\u007F\u0085\u00A0\u1680\u2000-\u200A\u2028\u2029\u202F\u205F\u3000]$" =>
            true
          _ => false
        }
        guard allowed else {
          raise self.invalid_escape("invalid identifier escape")
        }
        self.position = end
      }
      _ => break
    }
  }
  guard self.position > name_start else {
    raise self.syntax_error("identifier expected")
  }
  self.token = Identifier
  self.keyword = NotKeyword
  self.end = self.position
}

///|
fn identifier_escape(
  source : String,
  start : Int,
) -> (Char, Int) raise ParseError {
  guard start + 2 < source.length() else {
    raise InvalidEscape(
      position_at(source, start),
      "expected Unicode identifier escape",
    )
  }
  let (code_point, end) = lexmatch
    source.view(start_offset=start + 1) with longest {
    (re"^u[{]", after=digits) =>
      read_braced_identifier_escape(source, digits.start_offset())
    (re"^u", after=digits) =>
      read_fixed_identifier_escape(source, digits.start_offset())
    _ =>
      raise InvalidEscape(
        position_at(source, start),
        "expected Unicode identifier escape",
      )
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
  lexmatch source.view(start_offset=digits_start) with longest {
    (re"^[0-9A-Fa-f]{0,4}" as digits, after=rest) => {
      guard digits.length() == 4 else {
        raise InvalidEscape(
          position_at(source, rest.start_offset()),
          "invalid Unicode escape",
        )
      }
      (decode_identifier_code_point(source, digits), rest.start_offset())
    }
  }
}

///|
fn read_braced_identifier_escape(
  source : String,
  digits_start : Int,
) -> (Int, Int) raise ParseError {
  lexmatch source.view(start_offset=digits_start) with longest {
    (re"^[0-9A-Fa-f]*" as digits, after=rest) => {
      let code_point = decode_identifier_code_point(source, digits)
      lexmatch rest {
        (re"^[}]", after=tail) => {
          guard digits.length() > 0 else {
            raise InvalidEscape(
              position_at(source, rest.start_offset()),
              "empty Unicode escape",
            )
          }
          (code_point, tail.start_offset())
        }
        _ =>
          raise InvalidEscape(
            position_at(source, rest.start_offset()),
            "invalid Unicode escape",
          )
      }
    }
  }
}

///|
fn decode_identifier_code_point(
  source : String,
  digits : StringView,
) -> Int raise ParseError {
  let significant = lexmatch digits {
    (re"^0+", after=rest) => rest
    _ => digits
  }
  // Unicode uses at most six significant hex digits. Point range errors at
  // the first digit that makes the value exceed the Unicode limit.
  let prefix_length = if significant.length() > 6 {
    6
  } else {
    significant.length()
  }
  let prefix_end = significant.start_offset() + prefix_length
  let prefix = source.view(
    start_offset=significant.start_offset(),
    end_offset=prefix_end,
  )
  guard decode_escape_digits(prefix, 16) is Some(code_point) else {
    raise InvalidEscape(
      position_at(source, prefix_end),
      "invalid Unicode escape",
    )
  }
  guard code_point <= 0x10FFFF else {
    raise InvalidEscape(
      position_at(source, prefix_end - 1),
      "invalid Unicode escape",
    )
  }
  guard significant.length() <= prefix_length else {
    raise InvalidEscape(
      position_at(source, prefix_end),
      "invalid Unicode escape",
    )
  }
  code_point
}

///|
/// Callers recognize the digit syntax; this step converts its numeric value.
fn decode_escape_digits(digits : StringView, radix : Int) -> Int? {
  let significant = lexmatch digits {
    (re"^0+", after=rest) => rest
    _ => digits
  }
  guard significant.length() > 0 else { return Some(0) }
  Some(@string.parse_int(significant, base=radix)) catch {
    _ => None
  }
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
    lexmatch source.view(start_offset=position, end_offset=end) {
      (re"^\\", after=_) => {
        let (character, next) = identifier_escape(source, position)
        output.write_char(character)
        continue next
      }
      _ => {
        let literal_end = identifier_literal_end(source, position, end)
        output.write_substring(source, position, literal_end - position)
        continue literal_end
      }
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
    guard literal_end < end && source.get_char(literal_end) is None else {
      break literal_end
    }
    continue literal_end + 1
  }
}
