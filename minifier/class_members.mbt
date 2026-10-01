///|
priv enum ClassPrefix {
  Plain
  Static
  Getter
  Setter
  Async
} derive(Eq)

///|
#valtype
priv struct ClassKey {
  node : Int
  computed : Bool
  prefix : ClassPrefix
}

///|
fn Parser::class_key(self : Parser) -> ClassKey raise ParseError {
  let computed = self.lexer.token == LeftBracket
  let prefix : ClassPrefix = match self.lexer.keyword {
    Static => ClassPrefix::Static
    Get => Getter
    Set => Setter
    Async => ClassPrefix::Async
    _ => Plain
  }
  let node = self.property_key()
  { node, computed, prefix, }
}

///|
fn ClassKey::flags(self : ClassKey, is_static : Bool) -> Int {
  (if is_static { CLASS_MEMBER_STATIC } else { 0 }) |
  (if self.computed { CLASS_MEMBER_COMPUTED } else { 0 })
}

///|
fn Parser::class_method(
  self : Parser,
  key : ClassKey,
  is_static : Bool,
  is_async : Bool,
  is_generator : Bool,
  kind : Int,
) -> Int raise ParseError {
  let function = self.function_tail(-1, is_async, is_generator, false)
  self.program.node(Method, key.node, function, kind, key.flags(is_static))
}

///|
/// Contextual modifiers and computed keys are consumed once, without a parser
/// probe. Only async requires the following key to be on the same line.
fn Parser::class_member(
  self : Parser,
  members : PendingEdges,
) -> Unit raise ParseError {
  let is_generator = self.eat(Asterisk)
  let key = self.class_key()
  guard key.is_static_modifier(self.lexer.token, is_generator) else {
    members.push(self.class_member_tail(key, false, is_generator))
    return
  }
  if self.lexer.token == LeftBrace {
    members.push(self.class_static_block())
  } else {
    let is_generator = self.eat(Asterisk)
    let key = self.class_key()
    members.push(self.class_member_tail(key, true, is_generator))
  }
}

///|
fn ClassKey::is_static_modifier(
  self : ClassKey,
  following : Token,
  is_generator : Bool,
) -> Bool {
  !is_generator &&
  self.prefix == Static &&
  following != LeftParenthesis &&
  following != Assignment &&
  following != Semicolon &&
  following != RightBrace
}

///|
fn Parser::class_static_block(self : Parser) -> Int raise ParseError {
  let outer_scope = self.scope
  self.scope = self.program.push_scope(outer_scope, scope_function)
  let body = self.block()
  self.scope = outer_scope
  self.program.node(ClassStaticBlock, body, 0, 0, 0)
}

///|
fn Parser::class_member_tail(
  self : Parser,
  key : ClassKey,
  is_static : Bool,
  is_generator : Bool,
) -> Int raise ParseError {
  let following = self.lexer.token
  guard !is_generator && following != LeftParenthesis else {
    return self.class_method(
      key,
      is_static,
      false,
      is_generator,
      METHOD_ORDINARY,
    )
  }
  let property_follows = following == Identifier ||
    following == StringLiteral ||
    following == Number ||
    following == LeftBracket
  let is_accessor = key.prefix == Getter || key.prefix == Setter
  let is_async = key.prefix == Async && !self.lexer.newline_before
  let has_method_prefix = (is_accessor && property_follows) ||
    (is_async && (property_follows || following == Asterisk))
  if has_method_prefix {
    self.prefixed_class_method(key.prefix, is_static, is_async)
  } else {
    self.class_field(key, is_static)
  }
}

///|
fn Parser::prefixed_class_method(
  self : Parser,
  prefix : ClassPrefix,
  is_static : Bool,
  is_async : Bool,
) -> Int raise ParseError {
  let is_generator = is_async && self.eat(Asterisk)
  let key = self.class_key()
  guard self.lexer.token == LeftParenthesis else {
    raise self.unexpected("method parameter list")
  }
  let kind = match prefix {
    Getter => METHOD_GETTER
    Setter => METHOD_SETTER
    _ => METHOD_ORDINARY
  }
  self.class_method(key, is_static, is_async, is_generator, kind)
}

///|
fn Parser::class_field(
  self : Parser,
  key : ClassKey,
  is_static : Bool,
) -> Int raise ParseError {
  let initializer = if self.eat(Assignment) {
    self.assignment_with_in()
  } else {
    -1
  }
  let field_terminated = self.lexer.token == Semicolon ||
    self.lexer.token == RightBrace ||
    self.lexer.newline_before
  guard field_terminated else {
    raise self.unexpected("class field terminator")
  }
  self.eat_semicolon()
  self.program.node(ClassField, key.node, initializer, 0, key.flags(is_static))
}
