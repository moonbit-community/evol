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
  let (exponent_marker, is_integer) = for index = start, is_integer = true {
    guard index < end else { break (end, is_integer) }
    let character = source[index]
    if character == 'e' || character == 'E' {
      break (index, is_integer)
    }
    guard is_digit(character) || character == '.' else { return None }
    continue index + 1, is_integer && character != '.'
  }
  let can_omit_leading_zero = start + 2 < exponent_marker &&
    source[start] == '0' &&
    source[start + 1] == '.'
  Some({
    mantissa_start: if can_omit_leading_zero {
      start + 1
    } else {
      start
    },
    exponent_marker,
    is_integer,
  })
}

///|
fn integer_exponent_spelling(
  source : String,
  start : Int,
  end : Int,
) -> String? {
  // Leading-zero integers may be legacy octal.
  guard source[start] != '0' else { return None }
  let significant_end = for index = end {
    guard index > start + 1 && source[index - 1] == '0' else { break index }
    continue index - 1
  }
  let trailing_zeros = end - significant_end
  guard trailing_zeros > 0 else { return None }
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
  let sign = source[marker + 1]
  let digits_start = marker + (if sign == '+' || sign == '-' { 2 } else { 1 })
  guard digits_start < end else { return None }
  for index in digits_start..<end {
    guard is_digit(source[index]) else { return None }
  }
  let significant_start = for index = digits_start {
    guard index < end - 1 && source[index] == '0' else { break index }
    continue index + 1
  }
  let exponent_is_zero = significant_start == end - 1 &&
    source[significant_start] == '0'
  let needs_minus = sign == '-' && !exponent_is_zero
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
  let source = self.program.source
  guard decimal_spelling(source, start, end) is Some(decimal) else {
    self.span(start, end, true)
    return
  }
  if decimal.exponent_marker == end {
    // BigInt and other bases were excluded by the decimal scan.
    if decimal.is_integer &&
      integer_exponent_spelling(source, start, end) is Some(text) {
      self.text(text, true)
    } else {
      self.span(decimal.mantissa_start, end, true)
    }
  } else if shortened_exponent_spelling(source, start, end, decimal)
    is Some(text) {
    self.text(text, true)
  } else {
    self.span(start, end, true)
  }
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
    let is_escaped = source[index] == '\\'
    let character_index = index + (if is_escaped { 1 } else { 0 })
    let character = source[character_index]
    let is_quote = character == '\'' || character == '"'
    continue character_index + 1,
      unescaped_length - (if is_escaped && is_quote { 1 } else { 0 }),
      single_quotes + (if character == '\'' { 1 } else { 0 }),
      double_quotes + (if character == '"' { 1 } else { 0 })
  }
}

///|
fn QuoteCosts::preferred_quote(self : QuoteCosts, original : UInt16) -> UInt16 {
  if self.single_quotes < self.double_quotes {
    '\''
  } else if self.double_quotes < self.single_quotes {
    '"'
  } else {
    original
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
  guard end > start + 1 && (source[start] == '\'' || source[start] == '"') else {
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
    let is_escaped = source[index] == '\\'
    let character_index = index + (if is_escaped { 1 } else { 0 })
    let character = source[character_index]
    let is_quote = character == '\'' || character == '"'
    let needs_rewrite = (is_escaped && is_quote) ||
      (!is_escaped && character == quote)
    let next_index = character_index + 1
    guard needs_rewrite else { continue next_index, segment_start }
    text.write_substring(source, segment_start, index - segment_start)
    if character == quote {
      text.write_string("\\")
    }
    text.write_substring(source, character_index, 1)
    continue next_index, next_index
  }
  // Copy contiguous source spans so UTF-16 surrogate pairs are never split.
  text.write_substring(source, remaining, end - 1 - remaining)
  text.write_string(delimiter)
  text.to_string()
}
