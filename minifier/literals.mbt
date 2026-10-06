///|
priv struct DecimalSpelling {
  mantissa_start : Int
  exponent_marker : Int
  is_integer : Bool
}

///|
fn decimal_spelling(
  source : String,
  start : Int,
  end : Int,
) -> DecimalSpelling? {
  let spelling = source.view(start_offset=start, end_offset=end)
  let exponent_marker = lexmatch spelling with longest {
    (re"^[0-9.]*[eE]", after=rest) => rest.start_offset() - 1
    re"^[0-9.]*$" => end
    _ => return None
  }
  let mantissa = source.view(start_offset=start, end_offset=exponent_marker)
  let can_omit_leading_zero = mantissa =~ (re"^0\.[0-9.]", after=_)
  Some({
    mantissa_start: if can_omit_leading_zero {
      start + 1
    } else {
      start
    },
    exponent_marker,
    is_integer: mantissa =~ re"^[0-9]*$",
  })
}

///|
fn integer_exponent_spelling(
  source : String,
  start : Int,
  end : Int,
) -> String? {
  let spelling = source.view(start_offset=start, end_offset=end)
  // Leading-zero integers may be legacy octal.
  guard !(spelling =~ (re"^0", after=_)) else { return None }
  let trailing_zeros = lexmatch spelling {
    (re"0+$" as zeros, before=_) => zeros.length()
    _ => return None
  }
  let significant_end = end - trailing_zeros
  let exponent = trailing_zeros.to_string()
  let shortened_length = significant_end - start + 1 + exponent.length()
  guard shortened_length < end - start else { return None }
  let text = StringBuilder()
  text.write_substring(source, start, significant_end - start)
  text.write_string("e")
  text.write_string(exponent)
  Some(text.to_string())
}

///|
fn shortened_exponent_spelling(
  source : String,
  start : Int,
  end : Int,
  decimal : DecimalSpelling,
) -> String? {
  let marker = decimal.exponent_marker
  guard marker + 1 < end else { return None }
  let exponent = source.view(start_offset=marker + 1, end_offset=end)
  let (digits, has_minus) = lexmatch exponent with longest {
    (re"^-", after=digits) => (digits, true)
    (re"^\+", after=digits) => (digits, false)
    _ => (exponent, false)
  }
  guard digits =~ re"^[0-9]+$" else { return None }
  let (significant_start, exponent_is_zero) = lexmatch digits with longest {
    (re"^0+", after=rest) =>
      if rest.length() == 0 {
        (end - 1, true)
      } else {
        (rest.start_offset(), false)
      }
    _ => (digits.start_offset(), false)
  }
  let needs_minus = has_minus && !exponent_is_zero
  let mantissa_length = marker - decimal.mantissa_start
  let exponent_length = end -
    significant_start +
    (if needs_minus { 1 } else { 0 })
  guard mantissa_length + 1 + exponent_length < end - start else { return None }
  let text = StringBuilder()
  text.write_substring(source, decimal.mantissa_start, mantissa_length)
  text.write_string("e")
  if needs_minus {
    text.write_string("-")
  }
  text.write_substring(source, significant_start, end - significant_start)
  Some(text.to_string())
}

///|
/// Only rewrite decimal spellings whose numeric value is unchanged exactly.
fn Printer::number(self : Printer, start : Int, end : Int) -> Unit {
  // No supported numeric rewrite can shorten a one- or two-character spelling.
  guard end - start > 2 else { return self.span(start, end, true) }
  let source = self.program.source
  guard decimal_spelling(source, start, end) is Some(decimal) else {
    self.span(start, end, true)
    return
  }
  guard decimal.exponent_marker != end else {
    // BigInt and other bases were excluded by the decimal scan.
    guard decimal.is_integer &&
      integer_exponent_spelling(source, start, end) is Some(text) else {
      return self.span(decimal.mantissa_start, end, true)
    }
    return self.text(text, true)
  }
  guard shortened_exponent_spelling(source, start, end, decimal) is Some(text) else {
    return self.span(start, end, true)
  }
  self.text(text, true)
}

///|
priv struct QuoteCosts {
  unescaped_length : Int
  single_quotes : Int
  double_quotes : Int
}

///|
fn quote_costs(source : String, start : Int, end : Int) -> QuoteCosts {
  for index = start + 1, unescaped_length = end - start, single_quotes = 0, double_quotes = 0 {
    guard index < end - 1 else {
      break { unescaped_length, single_quotes, double_quotes, }
    }
    match quoted_string_piece(source, index, end) {
      Unchanged(next) =>
        continue next, unescaped_length, single_quotes, double_quotes
      SingleQuote(next) =>
        continue next, unescaped_length, single_quotes + 1, double_quotes
      DoubleQuote(next) =>
        continue next, unescaped_length, single_quotes, double_quotes + 1
      EscapedSingleQuote(next) =>
        continue next, unescaped_length - 1, single_quotes + 1, double_quotes
      EscapedDoubleQuote(next) =>
        continue next, unescaped_length - 1, single_quotes, double_quotes + 1
    }
  }
}

///|
priv enum QuotedStringPiece {
  Unchanged(Int)
  SingleQuote(Int)
  DoubleQuote(Int)
  EscapedSingleQuote(Int)
  EscapedDoubleQuote(Int)
}

///|
fn quoted_string_piece(
  source : String,
  start : Int,
  end : Int,
) -> QuotedStringPiece {
  lexmatch source.view(start_offset=start, end_offset=end) with longest {
    (re"^[^\\'\"]+", after=rest) => Unchanged(rest.start_offset())
    (re"^\\'", after=rest) => EscapedSingleQuote(rest.start_offset())
    (re"^\\\"", after=rest) => EscapedDoubleQuote(rest.start_offset())
    (re"^'", after=rest) => SingleQuote(rest.start_offset())
    (re"^\"", after=rest) => DoubleQuote(rest.start_offset())
    (re"^\\[\u0000-\u{10FFFF}]", after=rest) => Unchanged(rest.start_offset())
    // Regex character classes exclude isolated UTF-16 surrogates.
    (re"^\\", after=_) => Unchanged(start + 2)
    _ => Unchanged(start + 1)
  }
}

///|
fn QuoteCosts::preferred_quote(self : QuoteCosts, original : UInt16) -> UInt16 {
  guard self.single_quotes != self.double_quotes else { return original }
  if self.single_quotes < self.double_quotes {
    '\''
  } else {
    '"'
  }
}

///|
fn QuoteCosts::quoted_length(self : QuoteCosts, quote : UInt16) -> Int {
  self.unescaped_length +
  (if quote == '\'' { self.single_quotes } else { self.double_quotes })
}

///|
/// Change quote escapes only. Other escapes and line continuations retain
/// their source spelling, including escapes in directive-like strings.
fn Printer::string(self : Printer, start : Int, end : Int) -> Unit {
  let source = self.program.source
  guard end > start + 1 else {
    self.span(start, end, true)
    return
  }
  let spelling = source.view(start_offset=start, end_offset=end)
  guard spelling =~ (re"^['\"]", after=_) else {
    // Property keys and module aliases also use StringLiteral arena nodes.
    self.span(start, end, true)
    return
  }
  let costs = quote_costs(source, start, end)
  let quote = costs.preferred_quote(source[start])
  guard costs.quoted_length(quote) < end - start else {
    self.span(start, end, false)
    return
  }
  self.text(quoted_text(source, start, end, quote), false)
}

///|
fn quoted_text(
  source : String,
  start : Int,
  end : Int,
  quote : UInt16,
) -> String {
  let text = StringBuilder()
  let delimiter = if quote == '\'' { "'" } else { "\"" }
  text.write_string(delimiter)
  let remaining = for index = start + 1, segment_start = start + 1 {
    guard index < end - 1 else { break segment_start }
    let (next_index, character, is_escaped) : (Int, UInt16, Bool) = match
      quoted_string_piece(source, index, end) {
      Unchanged(next) => continue next, segment_start
      SingleQuote(next) => (next, '\'', false)
      DoubleQuote(next) => (next, '"', false)
      EscapedSingleQuote(next) => (next, '\'', true)
      EscapedDoubleQuote(next) => (next, '"', true)
    }
    guard is_escaped || character == quote else {
      continue next_index, segment_start
    }
    text.write_substring(source, segment_start, index - segment_start)
    if character == quote {
      text.write_string("\\")
    }
    text.write_substring(source, next_index - 1, 1)
    continue next_index, next_index
  }
  // Copy contiguous source spans so UTF-16 surrogate pairs are never split.
  text.write_substring(source, remaining, end - 1 - remaining)
  text.write_string(delimiter)
  text.to_string()
}
