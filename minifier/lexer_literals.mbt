///|
fn string_text_rest(input : StringView, quote : UInt16) -> StringView {
  if quote == '\'' {
    lexmatch input with longest {
      (re"^[^'\\\r\n]+", after=rest) => rest
      _ => input
    }
  } else {
    lexmatch input with longest {
      (re"^[^\"\\\r\n]+", after=rest) => rest
      _ => input
    }
  }
}

///|
fn Lexer::string(self : Lexer, quote : UInt16) -> Unit raise ParseError {
  self.position += 1
  while true {
    let input = self.source.view(start_offset=self.position)
    let remaining = string_text_rest(input, quote)
    self.position = remaining.start_offset()
    lexmatch remaining with longest {
      (re"^\\\r\n", after=rest) => self.position = rest.start_offset()
      (re"^\\[\u{0}-\u{10FFFF}]", after=rest) =>
        self.position = rest.start_offset()
      (re"^['\"]", after=rest) => {
        self.finish_token(StringLiteral, rest)
        return
      }
      (re"^[\r\n]", after=_) =>
        raise self.syntax_error("newline in string literal")
      (re"^\\", after=_) => {
        guard self.position + 1 < self.source.length() else {
          raise self.unterminated("string escape")
        }
        self.position += 2
      }
      _ => {
        guard self.position < self.source.length() else {
          raise self.unterminated("string literal")
        }
        self.position += 1
      }
    }
  }
}

///|
fn Lexer::regex(self : Lexer) -> Unit raise ParseError {
  self.position += 1
  let mut in_class = false
  while true {
    lexmatch self.source.view(start_offset=self.position) with longest {
      (re"^[^\\/\[\]\r\n\u{2028}\u{2029}]+", after=rest) =>
        self.position = rest.start_offset()
      (re"^\\[^\r\n\u{2028}\u{2029}]", after=rest) =>
        self.position = rest.start_offset()
      (re"^\\[\r\n\u{2028}\u{2029}]", after=_) =>
        raise self.unterminated("regular expression escape")
      (re"^\\", after=_) => {
        guard self.position + 1 < self.source.length() else {
          raise self.unterminated("regular expression escape")
        }
        self.position += 2
      }
      (re"^/", after=rest) => {
        self.position = rest.start_offset()
        guard !in_class else { continue }
        let flags_end = raw_identifier_end(self.source, self.position)
        self.finish_token(Regex, self.source.view(start_offset=flags_end))
        return
      }
      (re"^\[", after=rest) => {
        in_class = true
        self.position = rest.start_offset()
      }
      (re"^\]", after=rest) => {
        in_class = false
        self.position = rest.start_offset()
      }
      (re"^[\r\n\u{2028}\u{2029}]", after=_) =>
        raise self.syntax_error("newline in regular expression")
      _ => {
        guard self.position < self.source.length() else {
          raise self.unterminated("regular expression")
        }
        self.position += 1
      }
    }
  }
}

///|
fn Lexer::template_chunk(
  self : Lexer,
  is_start : Bool,
) -> Unit raise ParseError {
  let content_start = self.position
  while true {
    lexmatch self.source.view(start_offset=self.position) with longest {
      (re"^[^\\`$]+", after=rest) => self.position = rest.start_offset()
      (re"^\\[\u{0}-\u{10FFFF}]", after=rest) =>
        self.position = rest.start_offset()
      (re"^`", after=_) => {
        let token : Token = if is_start {
          TemplateNoSubstitution
        } else {
          TemplateTail
        }
        self.finish_template_chunk(content_start, 1, token)
        return
      }
      (re"^\$[{]", after=_) => {
        let token : Token = if is_start { TemplateHead } else { TemplateMiddle }
        self.finish_template_chunk(content_start, 2, token)
        return
      }
      (re"^\$", after=rest) => self.position = rest.start_offset()
      (re"^\\", after=_) => {
        guard self.position + 1 < self.source.length() else {
          raise self.unterminated("template escape")
        }
        self.position += 2
      }
      _ => {
        guard self.position < self.source.length() else {
          raise self.unterminated("template literal")
        }
        self.position += 1
      }
    }
  }
}

///|
fn Lexer::finish_template_chunk(
  self : Lexer,
  content_start : Int,
  delimiter_width : Int,
  token : Token,
) -> Unit {
  self.quasi_start = content_start
  self.quasi_end = self.position
  self.position += delimiter_width
  self.token = token
  self.end = self.position
}

///|
fn Lexer::resume_template(self : Lexer) -> Unit raise ParseError {
  self.start = self.position
  self.template_chunk(false)
}
