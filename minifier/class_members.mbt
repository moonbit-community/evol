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
  let computed = self.lx.tok == LBracket
  let prefix = if self.lx.is_kw("static") {
    Static
  } else if self.lx.is_kw("get") {
    Getter
  } else if self.lx.is_kw("set") {
    Setter
  } else if self.lx.is_kw("async") {
    Async
  } else {
    Plain
  }
  let node = self.parse_property_key()
  { node, computed, prefix, }
}

///|
fn ClassKey::flags(self : ClassKey, is_static : Bool) -> Int {
  (if is_static { 1 } else { 0 }) | (if self.computed { 2 } else { 0 })
}

///|
fn Parser::class_method(
  self : Parser,
  key : ClassKey,
  is_static : Bool,
  is_async : Int,
  is_generator : Int,
  kind : Int,
) -> Int raise ParseError {
  let function = self.parse_function_rest(-1, is_async, is_generator, false)
  self.prog.node(Method, key.node, function, kind, key.flags(is_static))
}

///|
/// Contextual modifiers and computed keys are consumed once, without a parser
/// probe. Only async requires the following key to be on the same line.
fn Parser::parse_class_member(
  self : Parser,
  members : Array[Int],
) -> Unit raise ParseError {
  let mut is_static = false
  let mut is_generator = if self.eat(Star) { 1 } else { 0 }
  let mut key = self.class_key()
  while true {
    let next = self.lx.tok
    if is_generator == 0 &&
      !is_static &&
      key.prefix == Static &&
      next != LParen &&
      next != Assign &&
      next != Semicolon &&
      next != RBrace {
      is_static = true
      if next == LBrace {
        let outer = self.scope
        self.scope = self.prog.push_scope(outer, scope_function)
        let body = self.parse_block()
        self.scope = outer
        members.push(self.prog.node(ClassStaticBlock, body, 0, 0, 0))
        return
      }
      is_generator = if self.eat(Star) { 1 } else { 0 }
      key = self.class_key()
      continue
    }
    if is_generator != 0 || next == LParen {
      members.push(self.class_method(key, is_static, 0, is_generator, 0))
      return
    }
    let property_follows = next == Ident ||
      next == Str ||
      next == Number ||
      next == LBracket
    let accessor = key.prefix == Getter || key.prefix == Setter
    let asynchronous = key.prefix == Async && !self.lx.newline_before
    if (accessor && property_follows) ||
      (asynchronous && (property_follows || next == Star)) {
      let generator = if asynchronous && self.eat(Star) { 1 } else { 0 }
      let following = self.class_key()
      if self.lx.tok == LParen {
        let kind = if key.prefix == Getter {
          1
        } else if key.prefix == Setter {
          2
        } else {
          0
        }
        members.push(
          self.class_method(
            following,
            is_static,
            if asynchronous {
              1
            } else {
              0
            },
            generator,
            kind,
          ),
        )
        return
      }
      raise Unexpected("method parameter list expected")
    }
    let initializer = if self.eat(Assign) { self.parse_assign_in() } else { -1 }
    guard self.lx.tok == Semicolon ||
      self.lx.tok == RBrace ||
      self.lx.newline_before else {
      raise Unexpected("class field terminator expected")
    }
    self.eat_semi()
    members.push(
      self.prog.node(ClassField, key.node, initializer, 0, key.flags(is_static)),
    )
    return
  }
}
