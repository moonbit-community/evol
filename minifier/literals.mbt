///|
/// Only rewrite decimal spellings whose numeric value is unchanged exactly.
fn Printer::number(self : Printer, start : Int, end : Int) -> Unit {
  let source = self.program.source
  let (exponent, integral) = for index = start, integral = true {
    guard index < end else { break (end, integral) }
    let c = source[index]
    if c == 'e' || c == 'E' {
      break (index, integral)
    }
    guard is_digit(c) || c == '.' else { break (-1, false) }
    continue index + 1, integral && c != '.'
  }
  guard exponent >= 0 else {
    self.span(start, end, true)
    return
  }
  let mantissa = if start + 2 < exponent &&
    source[start] == '0' &&
    source[start + 1] == '.' {
    start + 1
  } else {
    start
  }
  if exponent == end {
    // Leading-zero integers may be legacy octal. BigInt and other bases were
    // excluded by the decimal scan above.
    if integral && source[start] != '0' {
      let digits_end = for index = end {
        guard index > start + 1 && source[index - 1] == '0' else { break index }
        continue index - 1
      }
      let zeros = end - digits_end
      if zeros > 0 {
        let power = zeros.to_string()
        if digits_end - start + 1 + power.length() < end - start {
          let text = StringBuilder()
          text.write_substring(source, start, digits_end - start)
          text.write_string("e")
          text.write_string(power)
          self.text(text.to_string(), true)
          return
        }
      }
    }
    self.span(mantissa, end, true)
    return
  }
  guard exponent + 1 < end else {
    self.span(start, end, true)
    return
  }
  let sign = source[exponent + 1]
  let digits = exponent + (if sign == '+' || sign == '-' { 2 } else { 1 })
  let valid = for index = digits {
    guard index < end else { break index > digits }
    guard is_digit(source[index]) else { break false }
    continue index + 1
  }
  guard valid else {
    self.span(start, end, true)
    return
  }
  let significant = for index = digits {
    guard index < end - 1 && source[index] == '0' else { break index }
    continue index + 1
  }
  let negative = sign == '-' &&
    !(significant == end - 1 && source[significant] == '0')
  let length = exponent -
    mantissa +
    1 +
    (if negative { 1 } else { 0 }) +
    end -
    significant
  guard length < end - start else {
    self.span(start, end, true)
    return
  }
  let text = StringBuilder()
  text.write_substring(source, mantissa, exponent - mantissa)
  text.write_string("e")
  if negative {
    text.write_string("-")
  }
  text.write_substring(source, significant, end - significant)
  self.text(text.to_string(), true)
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
  let (base, singles, doubles) = for index = start + 1, base = end - start, singles = 0, doubles = 0 {
    guard index < end - 1 else { break (base, singles, doubles) }
    let escaped = source[index] == '\\'
    let c = source[index + (if escaped { 1 } else { 0 })]
    let quote = c == '\'' || c == '"'
    continue index + (if escaped { 2 } else { 1 }),
      base - (if escaped && quote { 1 } else { 0 }),
      singles + (if quote && c == '\'' { 1 } else { 0 }),
      doubles + (if quote && c == '"' { 1 } else { 0 })
  }
  let quote : UInt16 = if singles < doubles {
    '\''
  } else if doubles < singles {
    '"'
  } else {
    source[start]
  }
  let length = base + (if quote == '\'' { singles } else { doubles })
  guard length < end - start else {
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
  let remaining = for index = start + 1, segment = start + 1 {
    guard index < end - 1 else { break segment }
    if source[index] == '\\' {
      let c = source[index + 1]
      if c == '\'' || c == '"' {
        text.write_substring(source, segment, index - segment)
        if c == quote {
          text.write_string("\\")
        }
        text.write_substring(source, index + 1, 1)
        continue index + 2, index + 2
      }
      continue index + 2, segment
    }
    if source[index] == quote {
      text.write_substring(source, segment, index - segment)
      text.write_string("\\")
      text.write_substring(source, index, 1)
      continue index + 1, index + 1
    }
    continue index + 1, segment
  }
  // Copy contiguous source spans so UTF-16 surrogate pairs are never split.
  text.write_substring(source, remaining, end - 1 - remaining)
  text.write_string(delimiter)
  text.to_string()
}
