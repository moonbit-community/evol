///|
pub suberror ParseError {
  Unexpected(String)
}

///|
priv struct Parser {
  program : Program
  lexer : Lexer
  mut scope : Int
  mut allow_in : Bool
  mut in_generator : Bool
}

///|
fn Parser::new(source : String) -> Parser {
  {
    program: Program::new(source),
    lexer: Lexer::new(source),
    scope: 0,
    allow_in: true,
    in_generator: false,
  }
}

///|
fn Parser::advance(self : Parser) -> Unit raise ParseError {
  self.lexer.next()
}

///|
fn Parser::expect(self : Parser, t : Token) -> Unit raise ParseError {
  if self.lexer.token != t {
    raise Unexpected(
      "expected \{t.to_repr()}, got \{self.lexer.token.to_repr()}",
    )
  }
  self.advance()
}

///|
fn Parser::eat(self : Parser, t : Token) -> Bool raise ParseError {
  if self.lexer.token == t {
    self.advance()
    true
  } else {
    false
  }
}

///|
fn Parser::peek_token(self : Parser) -> Token raise ParseError {
  let snapshot = self.lexer.snapshot()
  self.lexer.next()
  let t = self.lexer.token
  self.lexer.restore(snapshot)
  t
}

///|
fn Parser::peek_is_keyword(
  self : Parser,
  keyword : String,
) -> Bool raise ParseError {
  let snapshot = self.lexer.snapshot()
  self.lexer.next()
  let r = !self.lexer.newline_before && self.lexer.is_keyword(keyword)
  self.lexer.restore(snapshot)
  r
}

///|
fn Parser::nearest_var_scope(self : Parser) -> Int {
  // Parsing scopes form a tree rooted at scope_module.
  for scope = self.scope {
    let kind = self.program.scopes[scope].kind
    if kind == scope_module || kind == scope_function {
      break scope
    }
    continue self.program.scopes[scope].parent
  }
}

///|
fn Parser::add_reference(
  self : Parser,
  scope_id : Int,
  s : Int,
  e : Int,
) -> Int raise ParseError {
  self.program.references.push({
    scope: scope_id,
    name_start: s,
    name_end: e,
    name: self.program.identifier(s, e),
    symbol: -1,
  })
  self.program.references.length() - 1
}

///|
fn Parser::declare_symbol(
  self : Parser,
  scope_id : Int,
  s : Int,
  e : Int,
) -> Int raise ParseError {
  let symbol = self.program.add_symbol(s, e, scope_id)
  let name = self.program.symbols[symbol].name
  // A var initializer writes through a same-name catch binding.
  let mut scope = self.scope
  while scope != scope_id && scope >= 0 {
    if self.program.scopes[scope].kind == scope_catch &&
      self.program.scope_members[scope].get(name) is Some(caught) {
      self.program.link_symbols(symbol, caught)
    }
    scope = self.program.scopes[scope].parent
  }
  let r = self.add_reference(self.scope, s, e)
  self.program.references[r] = { ..self.program.references[r], symbol, }
  r
}

///|
fn Parser::declare_pattern(
  self : Parser,
  scope_id : Int,
  s : Int,
  e : Int,
) -> Int raise ParseError {
  self.program.node(
    PatternIdentifier,
    self.declare_symbol(scope_id, s, e),
    0,
    0,
    0,
  )
}

// ---------------------------------------------------------------------------
// Patterns
// ---------------------------------------------------------------------------

///|
fn Parser::parse_pattern(
  self : Parser,
  scope_id : Int,
  allow_default : Bool,
) -> Int raise ParseError {
  let inner = self.parse_pattern_atom(scope_id)
  if allow_default && self.lexer.token == Assignment {
    self.advance()
    let d = self.parse_assignment_allowing_in()
    self.program.node(PatternDefault, inner, d, 0, 0)
  } else {
    inner
  }
}

///|
fn Parser::parse_pattern_atom(
  self : Parser,
  scope_id : Int,
) -> Int raise ParseError {
  match self.lexer.token {
    Identifier => {
      let s = self.lexer.start
      let e = self.lexer.end
      self.advance()
      self.declare_pattern(scope_id, s, e)
    }
    LeftBracket => self.parse_array_pattern(scope_id)
    LeftBrace => self.parse_object_pattern(scope_id)
    _ => raise Unexpected("pattern expected, got \{self.lexer.token.to_repr()}")
  }
}

///|
fn Parser::parse_array_pattern(
  self : Parser,
  scope_id : Int,
) -> Int raise ParseError {
  self.expect(LeftBracket)
  let items : Array[Int] = []
  while self.lexer.token != RightBracket && self.lexer.token != EndOfFile {
    if self.lexer.token == Comma {
      self.advance()
      items.push(-1)
      continue
    }
    if self.lexer.token == Ellipsis {
      self.advance()
      let inner = self.parse_pattern(scope_id, false)
      items.push(self.program.node(PatternRest, inner, 0, 0, 0))
    } else {
      items.push(self.parse_pattern(scope_id, true))
    }
    if !self.eat(Comma) {
      break
    }
  }
  self.expect(RightBracket)
  let (offset, n) = self.program.edge_list(items)
  self.program.node(PatternArray, offset, n, 0, 0)
}

///|
fn Parser::parse_object_pattern(
  self : Parser,
  scope_id : Int,
) -> Int raise ParseError {
  self.expect(LeftBrace)
  let items : Array[Int] = []
  while self.lexer.token != RightBrace && self.lexer.token != EndOfFile {
    if self.lexer.token == Ellipsis {
      self.advance()
      let inner = self.parse_pattern(scope_id, false)
      items.push(self.program.node(PatternRest, inner, 0, 0, 0))
    } else {
      items.push(self.parse_pattern_property(scope_id))
    }
    if !self.eat(Comma) {
      break
    }
  }
  self.expect(RightBrace)
  let (offset, n) = self.program.edge_list(items)
  self.program.node(PatternObject, offset, n, 0, 0)
}

///|
fn Parser::parse_pattern_property(
  self : Parser,
  scope_id : Int,
) -> Int raise ParseError {
  let mut computed = 0
  let mut key = -1
  if self.lexer.token == LeftBracket {
    self.advance()
    key = self.parse_assignment_allowing_in()
    self.expect(RightBracket)
    computed = 1
  } else {
    key = self.parse_property_key()
  }
  if self.eat(Colon) {
    let v = self.parse_pattern(scope_id, true)
    return self.program.node(PatternProperty, key, v, 0, computed)
  }
  let key_start = self.program.nodes[key].a
  let key_end = self.program.nodes[key].b
  let v = self.declare_pattern(scope_id, key_start, key_end)
  let v2 = if self.lexer.token == Assignment {
    self.advance()
    let d = self.parse_assignment_allowing_in()
    self.program.node(PatternDefault, v, d, 0, 0)
  } else {
    v
  }
  self.program.node(PatternProperty, key, v2, 1, 0)
}

// ---------------------------------------------------------------------------
// Program / statements
// ---------------------------------------------------------------------------

///|
fn parse_program(source : String) -> Program raise ParseError {
  let p = Parser::new(source)
  p.advance()
  let statements = p.parse_statement_list(true)
  let (offset, n) = p.program.edge_list(statements)
  p.program.root = p.program.node(Block, offset, n, 0, 0)
  guard p.program.cover_defaults == 0 else {
    raise Unexpected("shorthand initializer requires a binding pattern")
  }
  p.program
}

///|
fn Parser::parse_statement_list(
  self : Parser,
  top : Bool,
) -> Array[Int] raise ParseError {
  let output : Array[Int] = []
  while self.lexer.token != EndOfFile && self.lexer.token != RightBrace {
    output.push(self.parse_statement())
  }
  guard !top || self.lexer.token == EndOfFile else {
    raise Unexpected("unexpected top-level }")
  }
  output
}

///|
fn Parser::parse_statement(self : Parser) -> Int raise ParseError {
  if self.lexer.token == LeftBrace {
    return self.parse_block()
  }
  if self.lexer.token == Semicolon {
    self.advance()
    return self.program.node(Empty, 0, 0, 0, 0)
  }
  if self.lexer.token == Identifier {
    if self.lexer.is_keyword("import") {
      let next = self.peek_token()
      if next != LeftParenthesis && next != Dot {
        return self.parse_import()
      }
    }
    if self.lexer.is_keyword("export") {
      return self.parse_export()
    }
    if self.lexer.is_keyword("var") {
      self.advance()
      return self.parse_variable_declaration(0)
    }
    if self.lexer.is_keyword("let") && self.peek_token() != Colon {
      self.advance()
      return self.parse_variable_declaration(1)
    }
    if self.lexer.is_keyword("const") {
      self.advance()
      return self.parse_variable_declaration(2)
    }
    if self.lexer.is_keyword("async") && self.peek_is_keyword("function") {
      self.advance()
      self.advance()
      return self.parse_function(true, is_async=1)
    }
    if self.lexer.is_keyword("function") {
      self.advance()
      return self.parse_function(true)
    }
    if self.lexer.is_keyword("class") {
      self.advance()
      return self.parse_class(true)
    }
    if self.lexer.is_keyword("if") {
      self.advance()
      return self.parse_if()
    }
    if self.lexer.is_keyword("for") {
      self.advance()
      return self.parse_for()
    }
    if self.lexer.is_keyword("while") {
      self.advance()
      return self.parse_while()
    }
    if self.lexer.is_keyword("do") {
      self.advance()
      return self.parse_do_while()
    }
    if self.lexer.is_keyword("return") {
      self.advance()
      return self.parse_return()
    }
    if self.lexer.is_keyword("throw") {
      self.advance()
      return self.parse_throw()
    }
    if self.lexer.is_keyword("break") {
      self.advance()
      return self.parse_break_continue(true)
    }
    if self.lexer.is_keyword("continue") {
      self.advance()
      return self.parse_break_continue(false)
    }
    if self.lexer.is_keyword("switch") {
      self.advance()
      return self.parse_switch()
    }
    if self.lexer.is_keyword("try") {
      self.advance()
      return self.parse_try()
    }
    if self.lexer.is_keyword("debugger") {
      self.advance()
      self.eat_semicolon()
      return self.program.node(Debugger, 0, 0, 0, 0)
    }
    if self.lexer.is_keyword("with") {
      self.advance()
      return self.parse_with()
    }
    if self.peek_token() == Colon {
      return self.parse_label()
    }
  }
  let e = self.parse_expression()
  self.eat_semicolon()
  self.program.node(ExpressionStatement, e, 0, 0, 0)
}

///|
fn Parser::eat_semicolon(self : Parser) -> Unit raise ParseError {
  if self.lexer.token == Semicolon {
    self.advance()
  }
}

///|
fn Parser::parse_block(self : Parser) -> Int raise ParseError {
  self.expect(LeftBrace)
  let saved = self.scope
  self.scope = self.program.push_scope(saved, scope_block)
  let statements = self.parse_statement_list(false)
  self.scope = saved
  self.expect(RightBrace)
  let (offset, n) = self.program.edge_list(statements)
  self.program.node(Block, offset, n, 0, 0)
}

///|
fn Parser::parse_variable_declaration(
  self : Parser,
  kind : Int,
) -> Int raise ParseError {
  let declarations = self.parse_declarators(kind)
  self.eat_semicolon()
  let (offset, n) = self.program.edge_list(declarations)
  self.program.node(VariableDeclaration, kind, offset, n, 0)
}

///|
fn Parser::parse_if(self : Parser) -> Int raise ParseError {
  self.expect(LeftParenthesis)
  let condition = self.parse_expression()
  self.expect(RightParenthesis)
  let consequent = self.parse_statement()
  let alternate = if self.lexer.token == Identifier &&
    self.lexer.is_keyword("else") {
    self.advance()
    self.parse_statement()
  } else {
    -1
  }
  self.program.node(If, condition, consequent, alternate, 0)
}

///|
fn Parser::parse_while(self : Parser) -> Int raise ParseError {
  self.expect(LeftParenthesis)
  let condition = self.parse_expression()
  self.expect(RightParenthesis)
  let body = self.parse_statement()
  self.program.node(While, condition, body, 0, 0)
}

///|
fn Parser::parse_do_while(self : Parser) -> Int raise ParseError {
  let body = self.parse_statement()
  if !(self.lexer.token == Identifier && self.lexer.is_keyword("while")) {
    raise Unexpected("while expected")
  }
  self.advance()
  self.expect(LeftParenthesis)
  let condition = self.parse_expression()
  self.expect(RightParenthesis)
  self.eat_semicolon()
  self.program.node(DoWhile, body, condition, 0, 0)
}

///|
fn Parser::parse_for(self : Parser) -> Int raise ParseError {
  let is_await = self.lexer.is_keyword("await")
  if is_await {
    self.advance()
  }
  self.expect(LeftParenthesis)
  let saved = self.scope
  self.scope = self.program.push_scope(saved, scope_block)
  let mut initializer = -1
  if self.lexer.token == Semicolon {

  } else if self.lexer.token == Identifier &&
    (
      self.lexer.is_keyword("var") ||
      self.lexer.is_keyword("let") ||
      self.lexer.is_keyword("const")
    ) {
    let kind = if self.lexer.is_keyword("var") {
      0
    } else if self.lexer.is_keyword("let") {
      1
    } else {
      2
    }
    self.advance()
    self.allow_in = false
    initializer = self.parse_variable_declaration_without_semicolon(kind)
    self.allow_in = true
    if self.lexer.token == Identifier && self.lexer.is_keyword("of") {
      self.advance()
      let right = self.parse_assignment()
      self.expect(RightParenthesis)
      let body = self.parse_statement()
      self.scope = saved
      return self.program.node(
        ForOf,
        initializer,
        right,
        body,
        if is_await {
          1
        } else {
          0
        },
      )
    }
    if self.lexer.token == Identifier && self.lexer.is_keyword("in") {
      guard !is_await else { raise Unexpected("for await requires of") }
      self.advance()
      let right = self.parse_expression()
      self.expect(RightParenthesis)
      let body = self.parse_statement()
      self.scope = saved
      return self.program.node(ForIn, initializer, right, body, 0)
    }
  } else {
    self.allow_in = false
    let e = self.parse_expression()
    self.allow_in = true
    if self.lexer.token == Identifier && self.lexer.is_keyword("of") {
      self.assignment_pattern(e)
      self.advance()
      let right = self.parse_assignment()
      self.expect(RightParenthesis)
      let body = self.parse_statement()
      self.scope = saved
      return self.program.node(
        ForOf,
        e,
        right,
        body,
        if is_await {
          1
        } else {
          0
        },
      )
    }
    if self.lexer.token == Identifier && self.lexer.is_keyword("in") {
      guard !is_await else { raise Unexpected("for await requires of") }
      self.assignment_pattern(e)
      self.advance()
      let right = self.parse_expression()
      self.expect(RightParenthesis)
      let body = self.parse_statement()
      self.scope = saved
      return self.program.node(ForIn, e, right, body, 0)
    }
    initializer = e
  }
  guard !is_await else { raise Unexpected("for await requires of") }
  self.expect(Semicolon)
  let condition = if self.lexer.token == Semicolon {
    -1
  } else {
    self.parse_expression()
  }
  self.expect(Semicolon)
  let update = if self.lexer.token == RightParenthesis {
    -1
  } else {
    self.parse_expression()
  }
  self.expect(RightParenthesis)
  let body = self.parse_statement()
  self.scope = saved
  self.program.node(For, initializer, condition, update, body)
}

///|
fn Parser::parse_variable_declaration_without_semicolon(
  self : Parser,
  kind : Int,
) -> Int raise ParseError {
  let declarations = self.parse_declarators(kind)
  let (offset, n) = self.program.edge_list(declarations)
  self.program.node(VariableDeclaration, kind, offset, n, 0)
}

///|
fn Parser::parse_declarators(
  self : Parser,
  kind : Int,
) -> Array[Int] raise ParseError {
  let scope_id = if kind == 0 { self.nearest_var_scope() } else { self.scope }
  let declarations : Array[Int] = []
  while true {
    let pattern = self.parse_pattern(scope_id, false)
    let initializer = if self.lexer.token == Assignment {
      self.advance()
      self.parse_assignment()
    } else {
      -1
    }
    declarations.push(self.program.node(Declarator, pattern, initializer, 0, 0))
    if !self.eat(Comma) {
      break
    }
  }
  declarations
}

///|
fn Parser::parse_return(self : Parser) -> Int raise ParseError {
  let argument = if self.lexer.newline_before ||
    self.lexer.token == Semicolon ||
    self.lexer.token == RightBrace ||
    self.lexer.token == EndOfFile {
    -1
  } else {
    self.parse_expression()
  }
  self.eat_semicolon()
  self.program.node(Return, argument, 0, 0, 0)
}

///|
fn Parser::parse_throw(self : Parser) -> Int raise ParseError {
  if self.lexer.newline_before {
    raise Unexpected("newline after throw")
  }
  let argument = self.parse_expression()
  self.eat_semicolon()
  self.program.node(Throw, argument, 0, 0, 0)
}

///|
fn Parser::parse_break_continue(
  self : Parser,
  is_break : Bool,
) -> Int raise ParseError {
  let mut label_start = -1
  let mut label_end = -1
  if !self.lexer.newline_before && self.lexer.token == Identifier {
    label_start = self.lexer.start
    label_end = self.lexer.end
    self.advance()
  }
  self.eat_semicolon()
  self.program.node(
    if is_break {
      Break
    } else {
      Continue
    },
    label_start,
    label_end,
    0,
    0,
  )
}

///|
fn Parser::parse_label(self : Parser) -> Int raise ParseError {
  let label_start = self.lexer.start
  let label_end = self.lexer.end
  self.advance()
  self.expect(Colon)
  let body = self.parse_statement()
  self.program.node(Labeled, label_start, label_end, body, 0)
}

///|
fn Parser::parse_with(self : Parser) -> Int raise ParseError {
  self.expect(LeftParenthesis)
  let object = self.parse_expression()
  self.expect(RightParenthesis)
  let body = self.parse_statement()
  self.program.node(With, object, body, 0, 0)
}

///|
fn Parser::parse_switch(self : Parser) -> Int raise ParseError {
  self.expect(LeftParenthesis)
  let disc = self.parse_expression()
  self.expect(RightParenthesis)
  self.expect(LeftBrace)
  let saved = self.scope
  self.scope = self.program.push_scope(saved, scope_block)
  let cases : Array[Int] = []
  while self.lexer.token != RightBrace && self.lexer.token != EndOfFile {
    let condition = if self.lexer.token == Identifier &&
      self.lexer.is_keyword("case") {
      self.advance()
      self.parse_expression()
    } else if self.lexer.token == Identifier && self.lexer.is_keyword("default") {
      self.advance()
      -1
    } else {
      raise Unexpected("case/default expected")
    }
    self.expect(Colon)
    let statements : Array[Int] = []
    while self.lexer.token != RightBrace &&
          self.lexer.token != EndOfFile &&
          !(self.lexer.token == Identifier &&
          (self.lexer.is_keyword("case") || self.lexer.is_keyword("default"))) {
      statements.push(self.parse_statement())
    }
    let (statements_offset, statements_length) = self.program.edge_list(
      statements,
    )
    cases.push(
      self.program.node(
        SwitchCase,
        condition,
        statements_offset,
        statements_length,
        0,
      ),
    )
  }
  self.scope = saved
  self.expect(RightBrace)
  let (coff, cn) = self.program.edge_list(cases)
  self.program.node(Switch, disc, coff, cn, 0)
}

///|
fn Parser::parse_try(self : Parser) -> Int raise ParseError {
  let block = self.parse_block()
  let mut handler = -1
  if self.lexer.token == Identifier && self.lexer.is_keyword("catch") {
    self.advance()
    let saved = self.scope
    self.scope = self.program.push_scope(saved, scope_catch)
    let mut parameter = -1
    if self.eat(LeftParenthesis) {
      parameter = self.parse_pattern(self.scope, false)
      self.expect(RightParenthesis)
    }
    let body = self.parse_block()
    self.scope = saved
    handler = self.program.node(Catch, parameter, body, 0, 0)
  }
  let finalizer = if self.lexer.token == Identifier &&
    self.lexer.is_keyword("finally") {
    self.advance()
    self.parse_block()
  } else {
    -1
  }
  self.program.node(Try, block, handler, finalizer, 0)
}

// ---------------------------------------------------------------------------
// Functions and classes
// ---------------------------------------------------------------------------

///|
fn Parser::parse_function(
  self : Parser,
  is_declaration : Bool,
  is_async? : Int = 0,
) -> Int raise ParseError {
  let mut is_generator = 0
  if self.lexer.token == Asterisk {
    self.advance()
    is_generator = 1
  }
  let mut name_reference = -1
  if self.lexer.token == Identifier {
    let s = self.lexer.start
    let e = self.lexer.end
    self.advance()
    if is_declaration {
      name_reference = self.declare_symbol(self.scope, s, e)
      if self.program.scopes[self.scope].kind == scope_block {
        // Sloppy-mode block functions can also expose an Annex B var binding.
        self.program.preserved_names[self.program.identifier(s, e)] = true
      }
    } else {
      name_reference = self.add_reference(self.scope, s, e)
    }
  }
  let function_id = self.parse_function_rest(
    name_reference,
    is_async,
    is_generator,
    !is_declaration,
  )
  self.program.node(
    if is_declaration {
      FunctionDeclaration
    } else {
      FunctionExpression
    },
    function_id,
    0,
    0,
    0,
  )
}

///|
fn Parser::parse_function_rest(
  self : Parser,
  name_reference : Int,
  is_async : Int,
  is_generator : Int,
  bind_name_inside : Bool,
) -> Int raise ParseError {
  let outer = self.scope
  let saved_generator = self.in_generator
  self.in_generator = is_generator != 0
  let parent = if bind_name_inside && name_reference >= 0 {
    let name_scope = self.program.push_scope(outer, scope_block)
    let r = self.program.references[name_reference]
    let symbol = self.program.add_symbol(r.name_start, r.name_end, name_scope)
    self.program.references[name_reference] = {
      ..r,
      symbol,
      scope: name_scope,
    }
    name_scope
  } else {
    outer
  }
  let function_scope = self.program.push_scope(parent, scope_parameters)
  self.scope = function_scope
  self.expect(LeftParenthesis)
  let parameters : Array[Int] = []
  while self.lexer.token != RightParenthesis && self.lexer.token != EndOfFile {
    if self.lexer.token == Ellipsis {
      self.advance()
      let inner = self.parse_pattern(function_scope, false)
      parameters.push(self.program.node(PatternRest, inner, 0, 0, 0))
    } else {
      parameters.push(self.parse_pattern(function_scope, true))
    }
    if !self.eat(Comma) {
      break
    }
  }
  self.expect(RightParenthesis)
  let body = self.parse_function_body()
  self.scope = outer
  self.in_generator = saved_generator
  let (offset, n) = self.program.edge_list(parameters)
  self.program.functions.push({
    name: name_reference,
    parameters_offset: offset,
    parameters_count: n,
    body,
    flags: is_async * function_flag_async +
    is_generator * function_flag_generator,
  })
  self.program.functions.length() - 1
}

///|
fn Parser::parse_class(
  self : Parser,
  is_declaration : Bool,
) -> Int raise ParseError {
  let mut name_reference = -1
  if self.lexer.token == Identifier && !self.lexer.is_keyword("extends") {
    let s = self.lexer.start
    let e = self.lexer.end
    self.advance()
    if is_declaration {
      name_reference = self.declare_symbol(self.scope, s, e)
    } else {
      name_reference = self.add_reference(self.scope, s, e)
    }
  }
  let outer = self.scope
  let class_scope = self.program.push_scope(outer, scope_class)
  self.scope = class_scope
  if !is_declaration && name_reference >= 0 {
    let r = self.program.references[name_reference]
    let symbol = self.program.add_symbol(r.name_start, r.name_end, class_scope)
    self.program.references[name_reference] = {
      ..r,
      symbol,
      scope: class_scope,
    }
  }
  let mut superclass = -1
  if self.lexer.token == Identifier && self.lexer.is_keyword("extends") {
    self.advance()
    superclass = self.parse_postfix_expr()
  }
  self.expect(LeftBrace)
  let members : Array[Int] = []
  while self.lexer.token != RightBrace && self.lexer.token != EndOfFile {
    if self.eat(Semicolon) {
      continue
    }
    self.parse_class_member(members)
  }
  self.scope = outer
  self.expect(RightBrace)
  let (offset, n) = self.program.edge_list(members)
  self.program.classes.push({
    name: name_reference,
    superclass,
    members_offset: offset,
    members_count: n,
  })
  let cid = self.program.classes.length() - 1
  self.program.node(
    if is_declaration {
      ClassDeclaration
    } else {
      ClassExpression
    },
    cid,
    0,
    0,
    0,
  )
}

///|
fn Parser::parse_property_key(self : Parser) -> Int raise ParseError {
  match self.lexer.token {
    Number => {
      let n = self.program.node(
        NumberLiteral,
        self.lexer.start,
        self.lexer.end,
        0,
        0,
      )
      self.advance()
      n
    }
    StringLiteral => {
      let n = self.program.node(
        StringLiteral,
        self.lexer.start,
        self.lexer.end,
        0,
        0,
      )
      self.advance()
      n
    }
    LeftBracket => {
      self.advance()
      let e = self.parse_assignment_allowing_in()
      self.expect(RightBracket)
      e
    }
    Identifier => {
      let n = self.program.node(
        StringLiteral,
        self.lexer.start,
        self.lexer.end,
        0,
        0,
      )
      self.advance()
      n
    }
    _ => raise Unexpected("property key expected")
  }
}

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

///|
fn Parser::parse_expression(self : Parser) -> Int raise ParseError {
  let first = self.parse_assignment()
  if self.lexer.token != Comma {
    return first
  }
  let items : Array[Int] = [first]
  while self.eat(Comma) {
    items.push(self.parse_assignment())
  }
  let (offset, n) = self.program.edge_list(items)
  self.program.node(Sequence, offset, n, 0, 0)
}

///|
fn Parser::parse_assignment(self : Parser) -> Int raise ParseError {
  if self.in_generator && self.lexer.is_keyword("yield") {
    self.advance()
    let delegated = !self.lexer.newline_before && self.eat(Asterisk)
    let operator = self.program.add_operator(
      if delegated {
        Operator::YieldStar
      } else {
        Operator::Yield
      },
    )
    let e = if (!delegated && self.lexer.newline_before) ||
      self.lexer.token == Semicolon ||
      self.lexer.token == RightBrace ||
      self.lexer.token == EndOfFile ||
      self.lexer.token == RightParenthesis ||
      self.lexer.token == RightBracket ||
      self.lexer.token == Comma {
      guard !delegated else {
        raise Unexpected("yield* requires an expression")
      }
      -1
    } else {
      self.parse_assignment()
    }
    return self.program.node(Unary, operator, e, 0, 0)
  }
  let left = self.parse_cond()
  match self.lexer.token {
    Assignment
    | AdditionAssignment
    | SubtractionAssignment
    | MultiplicationAssignment
    | DivisionAssignment
    | ModuloAssignment
    | ExponentiationAssignment
    | LeftShiftAssignment
    | RightShiftAssignment
    | UnsignedRightShiftAssignment
    | BitwiseAndAssignment
    | BitwiseOrAssignment
    | BitwiseXorAssignment
    | LogicalAndAssignment
    | LogicalOrAssignment
    | NullishCoalescingAssignment => {
      if self.lexer.token == Assignment {
        self.assignment_pattern(left)
      }
      let operator = self.assignment_operator()
      self.advance()
      let right = self.parse_assignment()
      self.program.node(Assignment, operator, left, right, 0)
    }
    _ => left
  }
}

///|
fn Parser::assignment_operator(self : Parser) -> Int {
  let o = match self.lexer.token {
    Assignment => Operator::Assignment
    AdditionAssignment => Operator::Add
    SubtractionAssignment => Operator::Subtract
    MultiplicationAssignment => Operator::Multiply
    DivisionAssignment => Operator::Divide
    ModuloAssignment => Operator::Modulo
    ExponentiationAssignment => Operator::Exponent
    LeftShiftAssignment => Operator::ShiftLeft
    RightShiftAssignment => Operator::ShiftRight
    UnsignedRightShiftAssignment => Operator::UnsignedShiftRight
    BitwiseAndAssignment => Operator::BitwiseAnd
    BitwiseOrAssignment => Operator::BitwiseOr
    BitwiseXorAssignment => Operator::BitwiseXor
    LogicalAndAssignment => Operator::LogicalAnd
    LogicalOrAssignment => Operator::LogicalOr
    NullishCoalescingAssignment => Operator::NullishCoalescing
    _ => Operator::Add
  }
  self.program.add_operator(o)
}

///|
fn Parser::parse_cond(self : Parser) -> Int raise ParseError {
  let condition = self.parse_binary(1)
  if self.lexer.token == Question {
    self.advance()
    let consequent = self.parse_assignment_allowing_in()
    self.expect(Colon)
    let alternate = self.parse_assignment()
    return self.program.node(Conditional, condition, consequent, alternate, 0)
  }
  condition
}

///|
fn Parser::binary_operator_info(self : Parser) -> (Int, Operator) {
  let (precedence, o) = match self.lexer.token {
    ExponentiationOperator => (11, Operator::Exponent)
    Asterisk => (10, Operator::Multiply)
    Slash => (10, Operator::Divide)
    Percent => (10, Operator::Modulo)
    Plus => (9, Operator::Add)
    Minus => (9, Operator::Subtract)
    ShiftLeft => (8, Operator::ShiftLeft)
    ShiftRight => (8, Operator::ShiftRight)
    UnsignedShiftRight => (8, Operator::UnsignedShiftRight)
    LessThan => (7, Operator::LessThan)
    GreaterThan => (7, Operator::GreaterThan)
    LessThanOrEqual => (7, Operator::LessThanOrEqual)
    GreaterThanOrEqual => (7, Operator::GreaterThanOrEqual)
    Equal => (6, Operator::Equal)
    NotEqual => (6, Operator::NotEqual)
    StrictlyEqual => (6, Operator::StrictlyEqual)
    StrictlyNotEqual => (6, Operator::StrictlyNotEqual)
    BitwiseAnd => (5, Operator::BitwiseAnd)
    BitwiseXor => (4, Operator::BitwiseXor)
    BitwiseOr => (3, Operator::BitwiseOr)
    LogicalAnd => (2, Operator::LogicalAnd)
    NullishCoalescing => (1, Operator::NullishCoalescing)
    LogicalOr => (1, Operator::LogicalOr)
    Identifier =>
      if self.allow_in && self.lexer.is_keyword("in") {
        (7, Operator::In)
      } else if self.lexer.is_keyword("instanceof") {
        (7, Operator::InstanceOf)
      } else {
        (-1, Operator::Add)
      }
    _ => (-1, Operator::Add)
  }
  (precedence, o)
}

///|
fn Parser::parse_binary(
  self : Parser,
  minimum_precedence : Int,
) -> Int raise ParseError {
  let mut left = self.parse_unary()
  while true {
    let (precedence, operation) = self.binary_operator_info()
    if precedence < minimum_precedence {
      break
    }
    let operator = self.program.add_operator(operation)
    self.advance()
    let next_minimum_precedence = if operation == Operator::Exponent {
      precedence
    } else {
      precedence + 1
    }
    let right = self.parse_binary(next_minimum_precedence)
    let logical = operation == Operator::LogicalAnd ||
      operation == Operator::LogicalOr ||
      operation == Operator::NullishCoalescing
    left = self.program.node(
      if logical {
        Logical
      } else {
        Binary
      },
      operator,
      left,
      right,
      0,
    )
  }
  left
}

///|
fn Parser::parse_unary(self : Parser) -> Int raise ParseError {
  match self.lexer.token {
    LogicalNot | Tilde | Plus | Minus => {
      let o = match self.lexer.token {
        LogicalNot => Operator::LogicalNot
        Tilde => Operator::BitwiseNot
        Plus => Operator::Plus
        _ => Operator::Minus
      }
      let operator = self.program.add_operator(o)
      self.advance()
      let e = self.parse_unary()
      self.program.node(Unary, operator, e, 0, 0)
    }
    Increment | Decrement => {
      let o = if self.lexer.token == Increment {
        Operator::Increment
      } else {
        Operator::Decrement
      }
      let operator = self.program.add_operator(o)
      self.advance()
      let e = self.parse_unary()
      self.program.node(Update, operator, 1, e, 0)
    }
    Identifier =>
      if self.lexer.is_keyword("typeof") {
        let operator = self.program.add_operator(Operator::TypeOf)
        self.advance()
        let e = self.parse_unary()
        self.program.node(Unary, operator, e, 0, 0)
      } else if self.lexer.is_keyword("void") {
        let operator = self.program.add_operator(Operator::Void)
        self.advance()
        let e = self.parse_unary()
        self.program.node(Unary, operator, e, 0, 0)
      } else if self.lexer.is_keyword("delete") {
        let operator = self.program.add_operator(Operator::Delete)
        self.advance()
        let e = self.parse_unary()
        self.program.node(Unary, operator, e, 0, 0)
      } else if self.lexer.is_keyword("await") {
        let operator = self.program.add_operator(Operator::Await)
        self.advance()
        let e = self.parse_unary()
        self.program.node(Unary, operator, e, 0, 0)
      } else {
        self.parse_postfix_expr()
      }
    _ => self.parse_postfix_expr()
  }
}

///|
fn Parser::parse_postfix_expr(self : Parser) -> Int raise ParseError {
  let mut e = self.parse_primary()
  e = self.parse_call_member_tail(e)
  if !self.lexer.newline_before &&
    (self.lexer.token == Increment || self.lexer.token == Decrement) {
    let o = if self.lexer.token == Increment {
      Operator::Increment
    } else {
      Operator::Decrement
    }
    let operator = self.program.add_operator(o)
    self.advance()
    e = self.program.node(Update, operator, 0, e, 0)
  }
  e
}

///|
fn Parser::parse_call_member_tail(
  self : Parser,
  start : Int,
) -> Int raise ParseError {
  let mut e = start
  while true {
    match self.lexer.token {
      Dot => {
        self.advance()
        let name_start = self.lexer.start
        let name_end = self.lexer.end
        self.advance()
        e = self.program.node(Member, e, name_start, name_end, 0)
      }
      OptionalChaining => {
        self.advance()
        if self.lexer.token == LeftParenthesis {
          let (offset, n) = self.parse_arguments()
          e = self.program.node(Call, e, offset, n, 1)
        } else if self.lexer.token == LeftBracket {
          self.advance()
          let index = self.parse_expression_allowing_in()
          self.expect(RightBracket)
          e = self.program.node(Index, e, index, 0, 1)
        } else {
          let name_start = self.lexer.start
          let name_end = self.lexer.end
          self.advance()
          e = self.program.node(Member, e, name_start, name_end, 1)
        }
      }
      LeftBracket => {
        self.advance()
        let index = self.parse_expression_allowing_in()
        self.expect(RightBracket)
        e = self.program.node(Index, e, index, 0, 0)
      }
      LeftParenthesis => {
        let mut callee = e
        while self.program.nodes[callee].kind == Parenthesized {
          callee = self.program.nodes[callee].a
        }
        let callee = self.program.nodes[callee]
        if callee.kind == Identifier &&
          self.program.references[callee.a].name == name_eval {
          self.program.has_direct_eval = true
        }
        let (offset, n) = self.parse_arguments()
        e = self.program.node(Call, e, offset, n, 0)
      }
      TemplateNoSubstitution | TemplateHead => {
        let t = self.parse_template_expression()
        e = self.program.node(TaggedTemplate, e, t, 0, 0)
      }
      _ => break
    }
  }
  e
}

///|
fn Parser::parse_arguments(self : Parser) -> (Int, Int) raise ParseError {
  self.expect(LeftParenthesis)
  let arguments : Array[Int] = []
  while self.lexer.token != RightParenthesis && self.lexer.token != EndOfFile {
    if self.lexer.token == Ellipsis {
      self.advance()
      let inner = self.parse_assignment_allowing_in()
      arguments.push(self.program.node(Spread, inner, 0, 0, 0))
    } else {
      arguments.push(self.parse_assignment_allowing_in())
    }
    if !self.eat(Comma) {
      break
    }
  }
  self.expect(RightParenthesis)
  self.program.edge_list(arguments)
}

///|
fn Parser::parse_primary(self : Parser) -> Int raise ParseError {
  match self.lexer.token {
    Number => {
      let n = self.program.node(
        NumberLiteral,
        self.lexer.start,
        self.lexer.end,
        0,
        0,
      )
      self.advance()
      n
    }
    StringLiteral => {
      let n = self.program.node(
        StringLiteral,
        self.lexer.start,
        self.lexer.end,
        0,
        0,
      )
      self.advance()
      n
    }
    Slash | DivisionAssignment | Regex => {
      if self.lexer.token != Regex {
        self.lexer.position = self.lexer.start
        self.lexer.scan_regex()
      }
      let n = self.program.node(
        RegexLiteral,
        self.lexer.start,
        self.lexer.end,
        0,
        0,
      )
      self.advance()
      n
    }
    TemplateNoSubstitution | TemplateHead => self.parse_template_expression()
    LeftParenthesis => self.parse_parenthesized()
    LeftBracket => self.parse_array_literal()
    LeftBrace => self.parse_object_literal()
    Identifier => self.parse_identifier_expression()
    _ =>
      raise Unexpected("expression expected, got \{self.lexer.token.to_repr()}")
  }
}

///|
fn Parser::parse_identifier_expression(self : Parser) -> Int raise ParseError {
  if self.lexer.is_keyword("true") {
    self.advance()
    return self.program.node(BooleanLiteral, 1, 0, 0, 0)
  }
  if self.lexer.is_keyword("false") {
    self.advance()
    return self.program.node(BooleanLiteral, 0, 0, 0, 0)
  }
  if self.lexer.is_keyword("null") {
    self.advance()
    return self.program.node(NullLiteral, 0, 0, 0, 0)
  }
  if self.lexer.is_keyword("this") {
    self.advance()
    return self.program.node(This, 0, 0, 0, 0)
  }
  if self.lexer.is_keyword("super") {
    self.advance()
    return self.program.node(Super, 0, 0, 0, 0)
  }
  if self.lexer.is_keyword("function") {
    self.advance()
    return self.parse_function(false)
  }
  if self.lexer.is_keyword("class") {
    self.advance()
    return self.parse_class(false)
  }
  if self.lexer.is_keyword("new") {
    self.advance()
    return self.parse_new()
  }
  if self.lexer.is_keyword("async") {
    let p = self.peek_token()
    if p == Identifier || p == LeftParenthesis {
      let snapshot = self.lexer.snapshot()
      self.advance()
      let same_line = !self.lexer.newline_before
      if same_line &&
        self.lexer.token == Identifier &&
        self.lexer.is_keyword("function") {
        self.advance()
        return self.parse_function(false, is_async=1)
      }
      if same_line &&
        self.lexer.token == Identifier &&
        self.peek_token() == Arrow {
        let start = self.lexer.start
        let end = self.lexer.end
        self.advance()
        return self.parse_arrow_single(start, end, 1)
      }
      if same_line && self.lexer.token == LeftParenthesis {
        return self.parse_async_parenthesized(snapshot.start, snapshot.end)
      }
      self.lexer.restore(snapshot)
    }
  }
  let start = self.lexer.start
  let end = self.lexer.end
  self.advance()
  if self.lexer.token == Arrow {
    return self.parse_arrow_single(start, end, 0)
  }
  self.program.node(
    Identifier,
    self.add_reference(self.scope, start, end),
    0,
    0,
    0,
  )
}

///|
fn Parser::parse_new(self : Parser) -> Int raise ParseError {
  if self.lexer.token == Dot {
    self.advance()
    let name_start = self.lexer.start
    let name_end = self.lexer.end
    self.advance()
    return self.program.node(Member, -1, name_start, name_end, 2)
  }
  let callee = self.parse_member_only()
  let (offset, n) = if self.lexer.token == LeftParenthesis {
    self.parse_arguments()
  } else {
    (-1, 0)
  }
  self.program.node(New, callee, offset, n, 0)
}

///|
fn Parser::parse_member_only(self : Parser) -> Int raise ParseError {
  let mut e = self.parse_primary()
  while true {
    match self.lexer.token {
      Dot => {
        self.advance()
        let name_start = self.lexer.start
        let name_end = self.lexer.end
        self.advance()
        e = self.program.node(Member, e, name_start, name_end, 0)
      }
      LeftBracket => {
        self.advance()
        let index = self.parse_expression_allowing_in()
        self.expect(RightBracket)
        e = self.program.node(Index, e, index, 0, 0)
      }
      _ => break
    }
  }
  e
}

///|
fn Parser::parse_arrow_single(
  self : Parser,
  name_start : Int,
  name_end : Int,
  is_async : Int,
) -> Int raise ParseError {
  let outer = self.scope
  let saved_generator = self.in_generator
  self.in_generator = false
  let function_scope = self.program.push_scope(outer, scope_arrow_parameters)
  self.scope = function_scope
  guard !self.lexer.newline_before else {
    raise Unexpected("newline before arrow")
  }
  let parameter = self.declare_pattern(function_scope, name_start, name_end)
  let name_reference = -1
  self.expect(Arrow)
  let (body, expr_body) = self.parse_arrow_body()
  self.scope = outer
  self.in_generator = saved_generator
  let parameters : Array[Int] = [parameter]
  let (offset, n) = self.program.edge_list(parameters)
  self.program.functions.push({
    name: name_reference,
    parameters_offset: offset,
    parameters_count: n,
    body,
    flags: function_flag_arrow + is_async * function_flag_async + expr_body,
  })
  let function_id = self.program.functions.length() - 1
  self.program.node(ArrowExpression, function_id, 0, 0, 0)
}

///|
fn Parser::parse_arrow_body(self : Parser) -> (Int, Int) raise ParseError {
  if self.lexer.token == LeftBrace {
    (self.parse_function_body(), 0)
  } else {
    (self.parse_assignment(), function_flag_expression_body)
  }
}

///|
fn Parser::parse_parenthesized(self : Parser) -> Int raise ParseError {
  let expression = self.parenthesized()
  if self.lexer.token == Arrow {
    self.finish_arrow(expression, 0)
  } else {
    self.finish_parenthesized(expression)
  }
}

///|
fn Parser::parse_template_expression(self : Parser) -> Int raise ParseError {
  if self.lexer.token == TemplateNoSubstitution {
    let n = self.program.node(
      TemplateNoSubstitution,
      self.lexer.start,
      self.lexer.end,
      0,
      0,
    )
    self.advance()
    n
  } else {
    self.parse_template()
  }
}

///|
fn Parser::parse_template(self : Parser) -> Int raise ParseError {
  let parts : Array[Int] = []
  let mut quasi_count = 0
  while true {
    quasi_count = quasi_count + 1
    parts.push(self.lexer.quasi_start)
    parts.push(self.lexer.quasi_end)
    self.advance()
    let e = self.parse_expression_allowing_in()
    parts.push(e)
    if self.lexer.token != RightBrace {
      raise Unexpected("template substitution must end with }")
    }
    self.lexer.rescan_template_continue()
    if self.lexer.token == TemplateTail {
      quasi_count = quasi_count + 1
      parts.push(self.lexer.quasi_start)
      parts.push(self.lexer.quasi_end)
      self.advance()
      break
    }
  }
  let (offset, _n) = self.program.edge_list(parts)
  self.program.node(Template, offset, quasi_count, 0, 0)
}

///|
fn Parser::parse_array_literal(self : Parser) -> Int raise ParseError {
  self.expect(LeftBracket)
  let items : Array[Int] = []
  let mut trailing_comma = 0
  while self.lexer.token != RightBracket && self.lexer.token != EndOfFile {
    if self.lexer.token == Comma {
      self.advance()
      items.push(-1)
      continue
    }
    if self.lexer.token == Ellipsis {
      self.advance()
      let inner = self.parse_assignment_allowing_in()
      items.push(self.program.node(Spread, inner, 0, 0, 0))
    } else {
      items.push(self.parse_assignment_allowing_in())
    }
    if !self.eat(Comma) {
      break
    }
    trailing_comma = if self.lexer.token == RightBracket { 1 } else { 0 }
  }
  self.expect(RightBracket)
  let (offset, n) = self.program.edge_list(items)
  self.program.node(ArrayLiteral, offset, n, 0, trailing_comma)
}

///|
fn Parser::parse_object_literal(self : Parser) -> Int raise ParseError {
  self.expect(LeftBrace)
  let properties : Array[Int] = []
  let mut trailing_comma = 0
  while self.lexer.token != RightBrace && self.lexer.token != EndOfFile {
    properties.push(self.parse_object_property())
    if !self.eat(Comma) {
      break
    }
    trailing_comma = if self.lexer.token == RightBrace { 1 } else { 0 }
  }
  self.expect(RightBrace)
  let (offset, n) = self.program.edge_list(properties)
  self.program.node(ObjectLiteral, offset, n, 0, trailing_comma)
}

///|
fn Parser::parse_object_property(self : Parser) -> Int raise ParseError {
  if self.lexer.token == Ellipsis {
    self.advance()
    let inner = self.parse_assignment_allowing_in()
    return self.program.node(PatternRest, inner, 0, 0, 0)
  }
  let mut kind = 0
  if self.lexer.token == Identifier &&
    (self.lexer.is_keyword("get") || self.lexer.is_keyword("set")) &&
    self.peek_token() != Colon &&
    self.peek_token() != Assignment &&
    self.peek_token() != Comma &&
    self.peek_token() != RightBrace &&
    self.peek_token() != LeftParenthesis {
    kind = if self.lexer.is_keyword("get") { 1 } else { 2 }
    self.advance()
  }
  let mut is_async = 0
  let mut is_generator = 0
  if self.lexer.token == Identifier &&
    self.lexer.is_keyword("async") &&
    self.peek_token() != Colon &&
    self.peek_token() != Assignment &&
    self.peek_token() != Comma &&
    self.peek_token() != RightBrace &&
    self.peek_token() != LeftParenthesis {
    is_async = 1
    self.advance()
  }
  if self.lexer.token == Asterisk {
    self.advance()
    is_generator = 1
  }
  let was_ident = self.lexer.token == Identifier
  let mut computed = 0
  let key = if self.lexer.token == LeftBracket {
    self.advance()
    let k = self.parse_assignment_allowing_in()
    self.expect(RightBracket)
    computed = 1
    k
  } else {
    self.parse_property_key()
  }
  if self.lexer.token == LeftParenthesis {
    let function_id = self.parse_function_rest(
      -1, is_async, is_generator, false,
    )
    return self.program.node(Method, key, function_id, kind, computed)
  }
  if self.eat(Colon) {
    let v = self.parse_assignment_allowing_in()
    return self.program.node(Property, key, v, 0, computed)
  }
  if was_ident && computed == 0 {
    let r = self.add_reference(
      self.scope,
      self.program.nodes[key].a,
      self.program.nodes[key].b,
    )
    let binding = self.program.node(Identifier, r, 0, 0, 0)
    let value = if self.eat(Assignment) {
      let default_value = self.parse_assignment_allowing_in()
      self.program.cover_defaults += 1
      self.program.node(CoverDefault, binding, default_value, 0, 0)
    } else {
      binding
    }
    return self.program.node(Property, key, value, 1, 0)
  }
  let v = self.parse_assignment_allowing_in()
  self.program.node(Property, key, v, 0, computed)
}

///|
/// Parameter initializers cannot see declarations from the function body.
/// Body var declarations matching a parameter keep its spelling, preserving
/// JavaScript's initialization from the parameter environment.
fn Parser::parse_function_body(self : Parser) -> Int raise ParseError {
  let parameters = self.scope
  let saved_in = self.allow_in
  self.allow_in = true
  let body_scope = self.program.push_scope(parameters, scope_function)
  for name, symbol in self.program.scope_members[parameters] {
    self.program.scope_members[body_scope][name] = symbol
  }
  self.scope = body_scope
  self.expect(LeftBrace)
  let statements = self.parse_statement_list(false)
  self.expect(RightBrace)
  self.scope = parameters
  self.allow_in = saved_in
  let (offset, count) = self.program.edge_list(statements)
  self.program.node(Block, offset, count, 0, 0)
}

///|
fn Parser::parse_assignment_allowing_in(self : Parser) -> Int raise ParseError {
  let saved = self.allow_in
  self.allow_in = true
  let expression = self.parse_assignment()
  self.allow_in = saved
  expression
}

///|
fn Parser::parse_expression_allowing_in(self : Parser) -> Int raise ParseError {
  let saved = self.allow_in
  self.allow_in = true
  let expression = self.parse_expression()
  self.allow_in = saved
  expression
}
