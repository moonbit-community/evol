///|
priv struct Parser {
  program : Program
  lexer : Lexer
  pending_edges : Array[Int]
  mut scope : Int
  mut allow_in : Bool
  mut in_generator : Bool
}

///|
fn Parser::new(source : String) -> Parser {
  {
    program: Program::new(source),
    lexer: Lexer::new(source),
    pending_edges: [],
    scope: 0,
    allow_in: true,
    in_generator: false,
  }
}

///|
fn Parser::error_position(self : Parser) -> Position {
  position_at(self.lexer.source, self.lexer.start)
}

///|
/// The current token was not the expected one.
fn Parser::unexpected(self : Parser, expected : String) -> ParseError {
  UnexpectedToken(
    self.error_position(),
    expected,
    self.lexer.token.to_repr().to_string(),
  )
}

///|
/// Any syntax error that is not simply an unexpected token.
fn Parser::syntax_error(self : Parser, message : String) -> ParseError {
  Syntax(self.error_position(), message)
}

///|
fn Parser::advance(self : Parser) -> Unit raise ParseError {
  count_characters(
    self.program.source,
    self.lexer.start,
    self.lexer.end,
    self.program.character_counts,
    1,
  )
  self.lexer.next()
}

///|
fn Parser::expect(self : Parser, expected : Token) -> Unit raise ParseError {
  guard self.lexer.token == expected else {
    raise self.unexpected(expected.to_repr().to_string())
  }
  self.advance()
}

///|
fn Parser::eat(self : Parser, expected : Token) -> Bool raise ParseError {
  guard self.lexer.token == expected else { return false }
  self.advance()
  true
}

///|
fn Parser::peek_token(self : Parser) -> Token raise ParseError {
  let snapshot = self.lexer.snapshot()
  self.lexer.next()
  let token = self.lexer.token
  self.lexer.restore(snapshot)
  token
}

///|
fn Parser::peek_is_keyword(
  self : Parser,
  keyword : Keyword,
) -> Bool raise ParseError {
  let snapshot = self.lexer.snapshot()
  self.lexer.next()
  let matches_keyword = !self.lexer.newline_before &&
    self.lexer.keyword == keyword
  self.lexer.restore(snapshot)
  matches_keyword
}

///|
fn Parser::nearest_var_scope(self : Parser) -> Int {
  // Parsing scopes form a tree rooted at the module scope.
  for scope = self.scope {
    let kind = self.program.scopes[scope].kind
    guard kind != SCOPE_MODULE && kind != SCOPE_FUNCTION else { break scope }
    continue self.program.scopes[scope].parent
  }
}

///|
fn Parser::add_reference(
  self : Parser,
  scope_id : Int,
  name_start : Int,
  name_end : Int,
) -> Int raise ParseError {
  self.program.add_reference({
    scope: scope_id,
    name_start,
    name_end,
    name: self.program.identifier(name_start, name_end),
    symbol: -1,
  })
}

///|
fn Parser::declare_symbol(
  self : Parser,
  scope_id : Int,
  name_start : Int,
  name_end : Int,
) -> Int raise ParseError {
  let symbol = self.program.add_symbol(name_start, name_end, scope_id)
  let name = self.program.symbols[symbol].name
  // A var initializer writes through a same-name catch binding.
  let mut scope = self.scope
  while scope != scope_id && scope >= 0 {
    if self.program.scopes[scope].kind == SCOPE_CATCH &&
      self.program.scope_members[scope].get(name) is Some(caught) {
      self.program.link_symbols(symbol, caught)
    }
    scope = self.program.scopes[scope].parent
  }
  self.program.add_reference({
    scope: self.scope,
    name_start,
    name_end,
    name,
    symbol,
  })
}

///|
fn Parser::declare_pattern(
  self : Parser,
  scope_id : Int,
  name_start : Int,
  name_end : Int,
) -> Int raise ParseError {
  self.program.node(
    PatternIdentifier,
    self.declare_symbol(scope_id, name_start, name_end),
    0,
    0,
    0,
  )
}

// ---------------------------------------------------------------------------
// Patterns
// ---------------------------------------------------------------------------

///|
fn Parser::pattern(
  self : Parser,
  scope_id : Int,
  allow_default : Bool,
) -> Int raise ParseError {
  let inner = self.pattern_atom(scope_id)
  guard allow_default && self.lexer.token == Assignment else { return inner }
  self.advance()
  let default_value = self.assignment_with_in()
  self.program.node(PatternDefault, inner, default_value, 0, 0)
}

///|
fn Parser::pattern_atom(self : Parser, scope_id : Int) -> Int raise ParseError {
  match self.lexer.token {
    Identifier => {
      let name_start = self.lexer.start
      let name_end = self.lexer.end
      self.advance()
      self.declare_pattern(scope_id, name_start, name_end)
    }
    LeftBracket => self.array_pattern(scope_id)
    LeftBrace => self.object_pattern(scope_id)
    _ => raise self.unexpected("pattern")
  }
}

///|
fn Parser::array_pattern(self : Parser, scope_id : Int) -> Int raise ParseError {
  self.expect(LeftBracket)
  let items = self.begin_edges()
  while self.lexer.token != RightBracket && self.lexer.token != EndOfFile {
    guard self.lexer.token != Comma else {
      self.advance()
      items.push(-1)
      continue
    }
    if self.lexer.token == Ellipsis {
      self.advance()
      let inner = self.pattern(scope_id, false)
      items.push(self.program.node(PatternRest, inner, 0, 0, 0))
    } else {
      items.push(self.pattern(scope_id, true))
    }
    guard self.eat(Comma) else { break }
  }
  self.expect(RightBracket)
  let { offset, count, } = self.program.edge_list(items)
  self.program.node(PatternArray, offset, count, 0, 0)
}

///|
fn Parser::object_pattern(
  self : Parser,
  scope_id : Int,
) -> Int raise ParseError {
  self.expect(LeftBrace)
  let items = self.begin_edges()
  while self.lexer.token != RightBrace && self.lexer.token != EndOfFile {
    if self.lexer.token == Ellipsis {
      self.advance()
      let inner = self.pattern(scope_id, false)
      items.push(self.program.node(PatternRest, inner, 0, 0, 0))
    } else {
      items.push(self.pattern_property(scope_id))
    }
    guard self.eat(Comma) else { break }
  }
  self.expect(RightBrace)
  let { offset, count, } = self.program.edge_list(items)
  self.program.node(PatternObject, offset, count, 0, 0)
}

///|
fn Parser::pattern_property(
  self : Parser,
  scope_id : Int,
) -> Int raise ParseError {
  let (key, is_computed) = self.property_key_with_computed()
  guard !self.eat(Colon) else {
    let binding = self.pattern(scope_id, true)
    let flags = if is_computed { PROPERTY_COMPUTED } else { 0 }
    return self.program.node(PatternProperty, key, binding, 0, flags)
  }
  let key_start = self.program.nodes[key].a
  let key_end = self.program.nodes[key].b
  let binding = self.declare_pattern(scope_id, key_start, key_end)
  let value = if self.lexer.token == Assignment {
    self.advance()
    let default_value = self.assignment_with_in()
    self.program.node(PatternDefault, binding, default_value, 0, 0)
  } else {
    binding
  }
  self.program.node(PatternProperty, key, value, PROPERTY_SHORTHAND, 0)
}

///|
fn Parser::property_key_with_computed(
  self : Parser,
) -> (Int, Bool) raise ParseError {
  guard self.lexer.token == LeftBracket else {
    return (self.property_key(), false)
  }
  self.advance()
  let expression = self.assignment_with_in()
  self.expect(RightBracket)
  (expression, true)
}

///|
fn Program::parse(source : String) -> Program raise ParseError {
  let parser = Parser::new(source)
  parser.advance()
  let statements = parser.statements(true)
  let { offset, count, } = parser.program.edge_list(statements)
  parser.program.root = parser.program.node(Block, offset, count, 0, 0)
  guard parser.program.cover_defaults == 0 else {
    raise parser.syntax_error(
      "shorthand initializer requires a binding pattern",
    )
  }
  parser.program
}

///|
fn Parser::statements(
  self : Parser,
  is_top_level : Bool,
) -> PendingEdges raise ParseError {
  let output = self.begin_edges()
  while self.lexer.token != EndOfFile && self.lexer.token != RightBrace {
    output.push(self.statement())
  }
  guard !is_top_level || self.lexer.token == EndOfFile else {
    raise self.syntax_error("unexpected top-level }")
  }
  output
}

///|
fn Parser::statement(self : Parser) -> Int raise ParseError {
  match self.lexer.token {
    LeftBrace => self.block()
    Semicolon => {
      self.advance()
      self.program.node(Empty, 0, 0, 0, 0)
    }
    Identifier => self.identifier_statement()
    _ => self.expression_statement()
  }
}

///|
fn Parser::identifier_statement(self : Parser) -> Int raise ParseError {
  match self.lexer.keyword {
    Import if self.is_import_declaration() => self.parse_import()
    Export => self.parse_export()
    Var => {
      self.advance()
      self.declaration(DECLARATION_VAR)
    }
    Let if self.peek_token() != Colon => {
      self.advance()
      self.declaration(DECLARATION_LET)
    }
    Const => {
      self.advance()
      self.declaration(DECLARATION_CONST)
    }
    Async if self.peek_is_keyword(Keyword::Function) => {
      self.advance()
      self.advance()
      self.function(true, is_async=true)
    }
    Function => {
      self.advance()
      self.function(true)
    }
    Class => {
      self.advance()
      self.class(true)
    }
    If => {
      self.advance()
      self.parse_if()
    }
    For => {
      self.advance()
      self.parse_for()
    }
    While => {
      self.advance()
      self.parse_while()
    }
    Do => {
      self.advance()
      self.parse_do_while()
    }
    Return => {
      self.advance()
      self.parse_return()
    }
    Throw => {
      self.advance()
      self.parse_throw()
    }
    Break => {
      self.advance()
      self.jump(true)
    }
    Continue => {
      self.advance()
      self.jump(false)
    }
    Switch => {
      self.advance()
      self.parse_switch()
    }
    Try => {
      self.advance()
      self.parse_try()
    }
    Debugger => {
      self.advance()
      self.eat_semicolon()
      self.program.node(Debugger, 0, 0, 0, 0)
    }
    With => {
      self.advance()
      self.parse_with()
    }
    _ if self.peek_token() == Colon => self.label()
    _ => self.expression_statement()
  }
}

///|
fn Parser::is_import_declaration(self : Parser) -> Bool raise ParseError {
  let next = self.peek_token()
  next != LeftParenthesis && next != Dot
}

///|
fn Parser::expression_statement(self : Parser) -> Int raise ParseError {
  let expression = self.expression()
  self.eat_semicolon()
  self.program.node(ExpressionStatement, expression, 0, 0, 0)
}

///|
fn Parser::eat_semicolon(self : Parser) -> Unit raise ParseError {
  guard self.lexer.token == Semicolon else { return }
  self.advance()
}

///|
fn Parser::block(self : Parser) -> Int raise ParseError {
  self.expect(LeftBrace)
  let outer_scope = self.scope
  self.scope = self.program.push_scope(outer_scope, SCOPE_BLOCK)
  let statements = self.statements(false)
  self.scope = outer_scope
  self.expect(RightBrace)
  let { offset, count, } = self.program.edge_list(statements)
  self.program.node(Block, offset, count, 0, 0)
}

///|
fn Parser::declaration(self : Parser, kind : Int) -> Int raise ParseError {
  let declarations = self.declarators(kind)
  self.eat_semicolon()
  let { offset, count, } = self.program.edge_list(declarations)
  self.program.node(VariableDeclaration, kind, offset, count, 0)
}

///|
fn Parser::parse_if(self : Parser) -> Int raise ParseError {
  self.expect(LeftParenthesis)
  let condition = self.expression()
  self.expect(RightParenthesis)
  let consequent = self.statement()
  let alternate = if self.lexer.token == Identifier &&
    self.lexer.keyword == Keyword::Else {
    self.advance()
    self.statement()
  } else {
    -1
  }
  self.program.node(If, condition, consequent, alternate, 0)
}

///|
fn Parser::parse_while(self : Parser) -> Int raise ParseError {
  self.expect(LeftParenthesis)
  let condition = self.expression()
  self.expect(RightParenthesis)
  let body = self.statement()
  self.program.node(While, condition, body, 0, 0)
}

///|
fn Parser::parse_do_while(self : Parser) -> Int raise ParseError {
  let body = self.statement()
  guard self.lexer.token == Identifier && self.lexer.keyword == Keyword::While else {
    raise self.unexpected("while")
  }
  self.advance()
  self.expect(LeftParenthesis)
  let condition = self.expression()
  self.expect(RightParenthesis)
  self.eat_semicolon()
  self.program.node(DoWhile, body, condition, 0, 0)
}

///|
fn Parser::parse_for(self : Parser) -> Int raise ParseError {
  let is_await = self.lexer.keyword == Keyword::Await
  if is_await {
    self.advance()
  }
  self.expect(LeftParenthesis)
  let outer_scope = self.scope
  self.scope = self.program.push_scope(outer_scope, SCOPE_BLOCK)
  let (initializer, is_declaration) = self.for_initializer()
  match self.lexer.token {
    Identifier if self.lexer.keyword == Keyword::Of => {
      if !is_declaration {
        self.assignment_pattern(initializer)
      }
      self.for_of_tail(initializer, is_await, outer_scope)
    }
    Identifier if self.lexer.keyword == Keyword::In => {
      guard !is_await else { raise self.syntax_error("for await requires of") }
      if !is_declaration {
        self.assignment_pattern(initializer)
      }
      self.for_in_tail(initializer, outer_scope)
    }
    _ => {
      guard !is_await else { raise self.syntax_error("for await requires of") }
      self.classic_for_tail(initializer, outer_scope)
    }
  }
}

///|
fn Parser::for_initializer(self : Parser) -> (Int, Bool) raise ParseError {
  guard self.lexer.token != Semicolon else { return (-1, false) }
  let is_declaration = self.lexer.token == Identifier &&
    (
      self.lexer.keyword == Keyword::Var ||
      self.lexer.keyword == Keyword::Let ||
      self.lexer.keyword == Keyword::Const
    )
  let initializer = if is_declaration {
    let kind = match self.lexer.keyword {
      Var => DECLARATION_VAR
      Let => DECLARATION_LET
      _ => DECLARATION_CONST
    }
    self.advance()
    self.allow_in = false
    self.declaration_head(kind)
  } else {
    self.allow_in = false
    self.expression()
  }
  self.allow_in = true
  (initializer, is_declaration)
}

///|
fn Parser::for_of_tail(
  self : Parser,
  initializer : Int,
  is_await : Bool,
  outer_scope : Int,
) -> Int raise ParseError {
  self.advance()
  let iterable = self.assignment()
  self.expect(RightParenthesis)
  let body = self.statement()
  self.scope = outer_scope
  self.program.node(
    ForOf,
    initializer,
    iterable,
    body,
    if is_await {
      1
    } else {
      0
    },
  )
}

///|
fn Parser::for_in_tail(
  self : Parser,
  initializer : Int,
  outer_scope : Int,
) -> Int raise ParseError {
  self.advance()
  let object = self.expression()
  self.expect(RightParenthesis)
  let body = self.statement()
  self.scope = outer_scope
  self.program.node(ForIn, initializer, object, body, 0)
}

///|
fn Parser::classic_for_tail(
  self : Parser,
  initializer : Int,
  outer_scope : Int,
) -> Int raise ParseError {
  self.expect(Semicolon)
  let condition = if self.lexer.token == Semicolon {
    -1
  } else {
    self.expression()
  }
  self.expect(Semicolon)
  let update = if self.lexer.token == RightParenthesis {
    -1
  } else {
    self.expression()
  }
  self.expect(RightParenthesis)
  let body = self.statement()
  self.scope = outer_scope
  self.program.node(For, initializer, condition, update, body)
}

///|
fn Parser::declaration_head(self : Parser, kind : Int) -> Int raise ParseError {
  let declarations = self.declarators(kind)
  let { offset, count, } = self.program.edge_list(declarations)
  self.program.node(VariableDeclaration, kind, offset, count, 0)
}

///|
fn Parser::declarators(
  self : Parser,
  kind : Int,
) -> PendingEdges raise ParseError {
  let scope_id = if kind == DECLARATION_VAR {
    self.nearest_var_scope()
  } else {
    self.scope
  }
  let declarations = self.begin_edges()
  while true {
    let pattern = self.pattern(scope_id, false)
    let initializer = if self.lexer.token == Assignment {
      self.advance()
      self.assignment()
    } else {
      -1
    }
    declarations.push(self.program.node(Declarator, pattern, initializer, 0, 0))
    guard self.eat(Comma) else { break }
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
    self.expression()
  }
  self.eat_semicolon()
  self.program.node(Return, argument, 0, 0, 0)
}

///|
fn Parser::parse_throw(self : Parser) -> Int raise ParseError {
  guard !self.lexer.newline_before else {
    raise self.syntax_error("newline after throw")
  }
  let argument = self.expression()
  self.eat_semicolon()
  self.program.node(Throw, argument, 0, 0, 0)
}

///|
fn Parser::jump(self : Parser, is_break : Bool) -> Int raise ParseError {
  let (label_start, label_end, label_name) = if !self.lexer.newline_before &&
    self.lexer.token == Identifier {
    let start = self.lexer.start
    let end = self.lexer.end
    let name = self.program.identifier(start, end)
    self.advance()
    (start, end, name)
  } else {
    (-1, -1, -1)
  }
  self.eat_semicolon()
  let kind : NodeKind = if is_break { Break } else { Continue }
  self.program.node(kind, label_start, label_end, label_name, 0)
}

///|
fn Parser::label(self : Parser) -> Int raise ParseError {
  let label_start = self.lexer.start
  let label_end = self.lexer.end
  let label_name = self.program.identifier(label_start, label_end)
  self.advance()
  self.expect(Colon)
  let body = self.statement()
  self.program.node(Labeled, label_start, label_end, body, label_name)
}

///|
fn Parser::parse_with(self : Parser) -> Int raise ParseError {
  self.program.has_dynamic_scope = true
  self.expect(LeftParenthesis)
  let object = self.expression()
  self.expect(RightParenthesis)
  let body = self.statement()
  self.program.node(With, object, body, 0, 0)
}

///|
fn Parser::parse_switch(self : Parser) -> Int raise ParseError {
  self.expect(LeftParenthesis)
  let discriminant = self.expression()
  self.expect(RightParenthesis)
  self.expect(LeftBrace)
  let outer_scope = self.scope
  self.scope = self.program.push_scope(outer_scope, SCOPE_BLOCK)
  let cases = self.begin_edges()
  while self.lexer.token != RightBrace && self.lexer.token != EndOfFile {
    cases.push(self.switch_case())
  }
  self.scope = outer_scope
  self.expect(RightBrace)
  let { offset: cases_offset, count: cases_count, } = self.program.edge_list(
    cases,
  )
  self.program.node(Switch, discriminant, cases_offset, cases_count, 0)
}

///|
fn Parser::switch_case(self : Parser) -> Int raise ParseError {
  let condition = match self.lexer.token {
    Identifier if self.lexer.keyword == Keyword::Case => {
      self.advance()
      self.expression()
    }
    Identifier if self.lexer.keyword == Keyword::Default => {
      self.advance()
      -1
    }
    _ => raise self.unexpected("case or default")
  }
  self.expect(Colon)
  let statements = self.begin_edges()
  while !self.at_switch_case_end() {
    statements.push(self.statement())
  }
  let { offset, count, } = self.program.edge_list(statements)
  self.program.node(SwitchCase, condition, offset, count, 0)
}

///|
fn Parser::at_switch_case_end(self : Parser) -> Bool {
  self.lexer.token == RightBrace ||
  self.lexer.token == EndOfFile ||
  (
    self.lexer.token == Identifier &&
    (
      self.lexer.keyword == Keyword::Case ||
      self.lexer.keyword == Keyword::Default
    )
  )
}

///|
fn Parser::parse_try(self : Parser) -> Int raise ParseError {
  let block = self.block()
  let handler = if self.lexer.token == Identifier &&
    self.lexer.keyword == Keyword::Catch {
    self.catch_clause()
  } else {
    -1
  }
  let finalizer = if self.lexer.token == Identifier &&
    self.lexer.keyword == Keyword::Finally {
    self.advance()
    self.block()
  } else {
    -1
  }
  self.program.node(Try, block, handler, finalizer, 0)
}

///|
fn Parser::catch_clause(self : Parser) -> Int raise ParseError {
  self.advance()
  let outer_scope = self.scope
  self.scope = self.program.push_scope(outer_scope, SCOPE_CATCH)
  let parameter = if self.eat(LeftParenthesis) {
    let pattern = self.pattern(self.scope, false)
    self.expect(RightParenthesis)
    pattern
  } else {
    -1
  }
  let body = self.block()
  self.scope = outer_scope
  self.program.node(Catch, parameter, body, 0, 0)
}

///|
fn Parser::function(
  self : Parser,
  is_declaration : Bool,
  is_async? : Bool = false,
) -> Int raise ParseError {
  let is_generator = self.eat(Asterisk)
  let mut name_reference = -1
  if self.lexer.token == Identifier {
    let name_start = self.lexer.start
    let name_end = self.lexer.end
    self.advance()
    if is_declaration {
      name_reference = self.declare_symbol(self.scope, name_start, name_end)
      if self.program.scopes[self.scope].kind == SCOPE_BLOCK {
        // Sloppy-mode block functions can also expose an Annex B var binding.
        self.program.preserved_names[self.program.identifier(
          name_start, name_end,
        )] = true
      }
    } else {
      name_reference = self.add_reference(self.scope, name_start, name_end)
    }
  }
  let function_id = self.function_tail(
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
fn Parser::function_tail(
  self : Parser,
  name_reference : Int,
  is_async : Bool,
  is_generator : Bool,
  bind_name_inside : Bool,
) -> Int raise ParseError {
  let outer_scope = self.scope
  let saved_generator = self.in_generator
  self.in_generator = is_generator
  let parent = if bind_name_inside && name_reference >= 0 {
    self.function_name_scope(name_reference, outer_scope)
  } else {
    outer_scope
  }
  let function_scope = self.program.push_scope(parent, SCOPE_PARAMETERS)
  self.scope = function_scope
  let parameters = self.function_parameters(function_scope)
  let body = self.function_body()
  self.scope = outer_scope
  self.in_generator = saved_generator
  let { offset, count, } = self.program.edge_list(parameters)
  self.program.functions.push({
    name: name_reference,
    parameters_offset: offset,
    parameters_count: count,
    body,
    flags: function_modifier_flags(is_async, is_generator),
  })
  self.program.functions.length() - 1
}

///|
fn Parser::function_name_scope(
  self : Parser,
  name_reference : Int,
  outer_scope : Int,
) -> Int raise ParseError {
  let name_scope = self.program.push_scope(outer_scope, SCOPE_BLOCK)
  let reference = self.program.references[name_reference]
  let symbol = self.program.add_symbol(
    reference.name_start,
    reference.name_end,
    name_scope,
  )
  self.program.references[name_reference] = {
    ..reference,
    symbol,
    scope: name_scope,
  }
  name_scope
}

///|
fn Parser::function_parameters(
  self : Parser,
  function_scope : Int,
) -> PendingEdges raise ParseError {
  self.expect(LeftParenthesis)
  let parameters = self.begin_edges()
  while self.lexer.token != RightParenthesis && self.lexer.token != EndOfFile {
    if self.lexer.token == Ellipsis {
      self.advance()
      let inner = self.pattern(function_scope, false)
      parameters.push(self.program.node(PatternRest, inner, 0, 0, 0))
    } else {
      parameters.push(self.pattern(function_scope, true))
    }
    guard self.eat(Comma) else { break }
  }
  self.expect(RightParenthesis)
  parameters
}

///|
fn Parser::class(self : Parser, is_declaration : Bool) -> Int raise ParseError {
  let name_reference = self.class_name_reference(is_declaration)
  let outer_scope = self.scope
  self.enter_class_scope(name_reference, is_declaration)
  let superclass = self.class_superclass()
  let members = self.class_body_members()
  self.scope = outer_scope
  self.expect(RightBrace)
  self.finish_class(name_reference, superclass, members, is_declaration)
}

///|
fn Parser::class_name_reference(
  self : Parser,
  is_declaration : Bool,
) -> Int raise ParseError {
  let has_name = self.lexer.token == Identifier &&
    self.lexer.keyword != Keyword::Extends
  guard has_name else { return -1 }
  let name_start = self.lexer.start
  let name_end = self.lexer.end
  self.advance()
  guard is_declaration else {
    return self.add_reference(self.scope, name_start, name_end)
  }
  self.declare_symbol(self.scope, name_start, name_end)
}

///|
fn Parser::enter_class_scope(
  self : Parser,
  name_reference : Int,
  is_declaration : Bool,
) -> Unit raise ParseError {
  let class_scope = self.program.push_scope(self.scope, SCOPE_CLASS)
  self.scope = class_scope
  let has_inner_name = !is_declaration && name_reference >= 0
  guard has_inner_name else { return }
  let reference = self.program.references[name_reference]
  let symbol = self.program.add_symbol(
    reference.name_start,
    reference.name_end,
    class_scope,
  )
  self.program.references[name_reference] = {
    ..reference,
    symbol,
    scope: class_scope,
  }
}

///|
fn Parser::class_superclass(self : Parser) -> Int raise ParseError {
  let has_extends = self.lexer.token == Identifier &&
    self.lexer.keyword == Keyword::Extends
  guard has_extends else { return -1 }
  self.advance()
  self.postfix()
}

///|
/// Leave the closing brace unread so the caller restores the outer scope first.
fn Parser::class_body_members(self : Parser) -> PendingEdges raise ParseError {
  self.expect(LeftBrace)
  let members = self.begin_edges()
  while self.lexer.token != RightBrace && self.lexer.token != EndOfFile {
    guard !self.eat(Semicolon) else { continue }
    self.class_member(members)
  }
  members
}

///|
fn Parser::finish_class(
  self : Parser,
  name_reference : Int,
  superclass : Int,
  members : PendingEdges,
  is_declaration : Bool,
) -> Int {
  let { offset, count, } = self.program.edge_list(members)
  self.program.classes.push({
    name: name_reference,
    superclass,
    members_offset: offset,
    members_count: count,
  })
  let class_id = self.program.classes.length() - 1
  let kind = if is_declaration { ClassDeclaration } else { ClassExpression }
  self.program.node(kind, class_id, 0, 0, 0)
}

///|
fn Parser::property_key(self : Parser) -> Int raise ParseError {
  match self.lexer.token {
    Number => self.source_literal(NumberLiteral)
    StringLiteral | Identifier => self.source_literal(StringLiteral)
    LeftBracket => {
      self.advance()
      let expression = self.assignment_with_in()
      self.expect(RightBracket)
      expression
    }
    _ => raise self.unexpected("property key")
  }
}

///|
fn Parser::expression(self : Parser) -> Int raise ParseError {
  let first_expression = self.assignment()
  guard self.lexer.token == Comma else { return first_expression }
  let items = self.begin_edges()
  items.push(first_expression)
  while self.eat(Comma) {
    items.push(self.assignment())
  }
  let { offset, count, } = self.program.edge_list(items)
  self.program.node(Sequence, offset, count, 0, 0)
}

///|
fn Parser::assignment(self : Parser) -> Int raise ParseError {
  guard !(self.in_generator && self.lexer.keyword == Keyword::Yield) else {
    return self.yield_expression()
  }
  let left = self.conditional()
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
      let right = self.assignment()
      self.program.node(Assignment, operator, left, right, 0)
    }
    _ => left
  }
}

///|
fn Parser::yield_expression(self : Parser) -> Int raise ParseError {
  self.advance()
  let delegated = !self.lexer.newline_before && self.eat(Asterisk)
  let operation = if delegated { Operator::YieldStar } else { Operator::Yield }
  let operator = self.program.add_operator(operation)
  let has_no_argument = (!delegated && self.lexer.newline_before) ||
    self.lexer.token == Semicolon ||
    self.lexer.token == RightBrace ||
    self.lexer.token == EndOfFile ||
    self.lexer.token == RightParenthesis ||
    self.lexer.token == RightBracket ||
    self.lexer.token == Comma
  let argument = if has_no_argument {
    guard !delegated else {
      raise self.syntax_error("yield* requires an expression")
    }
    -1
  } else {
    self.assignment()
  }
  self.program.node(Unary, operator, argument, 0, 0)
}

///|
fn Parser::assignment_operator(self : Parser) -> Int {
  let operation = match self.lexer.token {
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
  self.program.add_operator(operation)
}

///|
fn Parser::conditional(self : Parser) -> Int raise ParseError {
  let condition = self.binary(1)
  guard self.lexer.token == Question else { return condition }
  self.advance()
  let consequent = self.assignment_with_in()
  self.expect(Colon)
  let alternate = self.assignment()
  self.program.node(Conditional, condition, consequent, alternate, 0)
}

///|
fn Parser::binary_operator(self : Parser) -> (Int, Operator) {
  match self.lexer.token {
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
    Identifier if self.allow_in && self.lexer.keyword == Keyword::In =>
      (7, Operator::In)
    Identifier if self.lexer.keyword == Keyword::InstanceOf =>
      (7, Operator::InstanceOf)
    _ => (-1, Operator::Add)
  }
}

///|
fn Parser::binary(self : Parser, min_precedence : Int) -> Int raise ParseError {
  for left = self.unary() {
    let (precedence, operation) = self.binary_operator()
    guard precedence >= min_precedence else { break left }
    let operator = self.program.add_operator(operation)
    self.advance()
    let next_precedence = if operation == Operator::Exponent {
      precedence
    } else {
      precedence + 1
    }
    let right = self.binary(next_precedence)
    let logical = operation == Operator::LogicalAnd ||
      operation == Operator::LogicalOr ||
      operation == Operator::NullishCoalescing
    continue self.program.node(
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
}

///|
fn Parser::unary(self : Parser) -> Int raise ParseError {
  match self.lexer.token {
    LogicalNot => self.prefix_unary(Operator::LogicalNot)
    Tilde => self.prefix_unary(Operator::BitwiseNot)
    Plus => self.prefix_unary(Operator::Plus)
    Minus => self.prefix_unary(Operator::Minus)
    Increment => self.prefix_update(Operator::Increment)
    Decrement => self.prefix_update(Operator::Decrement)
    Identifier if self.lexer.keyword == Keyword::TypeOf =>
      self.prefix_unary(Operator::TypeOf)
    Identifier if self.lexer.keyword == Keyword::Void =>
      self.prefix_unary(Operator::Void)
    Identifier if self.lexer.keyword == Keyword::Delete =>
      self.prefix_unary(Operator::Delete)
    Identifier if self.lexer.keyword == Keyword::Await =>
      self.prefix_unary(Operator::Await)
    _ => self.postfix()
  }
}

///|
fn Parser::prefix_unary(
  self : Parser,
  operation : Operator,
) -> Int raise ParseError {
  let operator = self.program.add_operator(operation)
  self.advance()
  let operand = self.unary()
  self.program.node(Unary, operator, operand, 0, 0)
}

///|
fn Parser::prefix_update(
  self : Parser,
  operation : Operator,
) -> Int raise ParseError {
  let operator = self.program.add_operator(operation)
  self.advance()
  let operand = self.unary()
  self.program.node(Update, operator, UPDATE_PREFIX, operand, 0)
}

///|
fn Parser::postfix(self : Parser) -> Int raise ParseError {
  let primary = self.primary()
  let expression = self.call_member_tail(primary)
  let has_postfix_update = !self.lexer.newline_before &&
    (self.lexer.token == Increment || self.lexer.token == Decrement)
  guard has_postfix_update else { return expression }
  let operation = if self.lexer.token == Increment {
    Operator::Increment
  } else {
    Operator::Decrement
  }
  let operator = self.program.add_operator(operation)
  self.advance()
  self.program.node(Update, operator, UPDATE_POSTFIX, expression, 0)
}

///|
fn Parser::call_member_tail(self : Parser, start : Int) -> Int raise ParseError {
  for expression = start {
    match self.lexer.token {
      Dot => {
        self.advance()
        continue self.named_member(expression, false)
      }
      OptionalChaining => {
        self.advance()
        continue self.optional_chain_tail(expression)
      }
      LeftBracket => continue self.index_member(expression, false)
      LeftParenthesis => {
        self.mark_direct_eval(expression)
        continue self.call_tail(expression, false)
      }
      TemplateNoSubstitution | TemplateHead => {
        let template = self.template_literal()
        continue self.program.node(TaggedTemplate, expression, template, 0, 0)
      }
      _ => break expression
    }
  }
}

///|
fn Parser::named_member(
  self : Parser,
  object : Int,
  is_optional : Bool,
) -> Int raise ParseError {
  let name_start = self.lexer.start
  let name_end = self.lexer.end
  self.advance()
  let flags = if is_optional { CHAIN_OPTIONAL } else { 0 }
  self.program.node(Member, object, name_start, name_end, flags)
}

///|
fn Parser::index_member(
  self : Parser,
  object : Int,
  is_optional : Bool,
) -> Int raise ParseError {
  self.advance()
  let index = self.expression_with_in()
  self.expect(RightBracket)
  let flags = if is_optional { CHAIN_OPTIONAL } else { 0 }
  self.program.node(Index, object, index, 0, flags)
}

///|
fn Parser::call_tail(
  self : Parser,
  callee : Int,
  is_optional : Bool,
) -> Int raise ParseError {
  let { offset, count, } = self.arguments()
  let flags = if is_optional { CHAIN_OPTIONAL } else { 0 }
  self.program.node(Call, callee, offset, count, flags)
}

///|
fn Parser::optional_chain_tail(
  self : Parser,
  object : Int,
) -> Int raise ParseError {
  match self.lexer.token {
    LeftParenthesis => self.call_tail(object, true)
    LeftBracket => self.index_member(object, true)
    _ => self.named_member(object, true)
  }
}

///|
fn Parser::mark_direct_eval(self : Parser, expression : Int) -> Unit {
  let callee = for current = expression {
    let node = self.program.nodes[current]
    guard node.kind == Parenthesized else { break node }
    continue node.a
  }
  guard callee.kind == Identifier &&
    self.program.references[callee.a].name == NAME_EVAL else {
    return
  }
  self.program.has_dynamic_scope = true
}

///|
fn Parser::arguments(self : Parser) -> EdgeRange raise ParseError {
  self.expect(LeftParenthesis)
  let arguments = self.begin_edges()
  while self.lexer.token != RightParenthesis && self.lexer.token != EndOfFile {
    if self.lexer.token == Ellipsis {
      self.advance()
      let inner = self.assignment_with_in()
      arguments.push(self.program.node(Spread, inner, 0, 0, 0))
    } else {
      arguments.push(self.assignment_with_in())
    }
    guard self.eat(Comma) else { break }
  }
  self.expect(RightParenthesis)
  self.program.edge_list(arguments)
}

///|
fn Parser::primary(self : Parser) -> Int raise ParseError {
  match self.lexer.token {
    Number => self.source_literal(NumberLiteral)
    StringLiteral => self.source_literal(StringLiteral)
    Slash | DivisionAssignment | Regex => {
      if self.lexer.token != Regex {
        self.lexer.position = self.lexer.start
        self.lexer.regex()
      }
      self.source_literal(RegexLiteral)
    }
    TemplateNoSubstitution | TemplateHead => self.template_literal()
    LeftParenthesis => self.parentheses()
    LeftBracket => self.array_literal()
    LeftBrace => self.object_literal()
    Identifier => self.identifier()
    _ => raise self.unexpected("expression")
  }
}

///|
fn Parser::source_literal(
  self : Parser,
  kind : NodeKind,
) -> Int raise ParseError {
  let literal = self.program.node(kind, self.lexer.start, self.lexer.end, 0, 0)
  self.advance()
  literal
}

///|
fn Parser::identifier(self : Parser) -> Int raise ParseError {
  match self.lexer.keyword {
    True => {
      self.advance()
      self.program.node(BooleanLiteral, 1, 0, 0, 0)
    }
    False => {
      self.advance()
      self.program.node(BooleanLiteral, 0, 0, 0, 0)
    }
    Null => {
      self.advance()
      self.program.node(NullLiteral, 0, 0, 0, 0)
    }
    This => {
      self.advance()
      self.program.node(This, 0, 0, 0, 0)
    }
    Super => {
      self.advance()
      self.program.node(Super, 0, 0, 0, 0)
    }
    Function => {
      self.advance()
      self.function(false)
    }
    Class => {
      self.advance()
      self.class(false)
    }
    New => {
      self.advance()
      self.parse_new()
    }
    Async => {
      guard self.async_expression() is Some(expression) else {
        return self.identifier_reference()
      }
      expression
    }
    _ => self.identifier_reference()
  }
}

///|
fn Parser::async_expression(self : Parser) -> Int? raise ParseError {
  let next_token = self.peek_token()
  guard next_token == Identifier || next_token == LeftParenthesis else {
    return None
  }
  let snapshot = self.lexer.snapshot()
  self.advance()
  let same_line = !self.lexer.newline_before
  match self.lexer.token {
    Identifier if same_line && self.lexer.keyword == Keyword::Function => {
      self.advance()
      Some(self.function(false, is_async=true))
    }
    Identifier if same_line && self.peek_token() == Arrow => {
      let start = self.lexer.start
      let end = self.lexer.end
      self.advance()
      Some(self.single_arrow(start, end, true))
    }
    LeftParenthesis if same_line =>
      Some(self.async_parentheses(snapshot.start, snapshot.end))
    _ => {
      self.lexer.restore(snapshot)
      None
    }
  }
}

///|
fn Parser::identifier_reference(self : Parser) -> Int raise ParseError {
  let start = self.lexer.start
  let end = self.lexer.end
  self.advance()
  guard self.lexer.token != Arrow else {
    return self.single_arrow(start, end, false)
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
  guard self.lexer.token != Dot else {
    self.advance()
    let name_start = self.lexer.start
    let name_end = self.lexer.end
    self.advance()
    return self.program.node(
      Member,
      -1,
      name_start,
      name_end,
      MEMBER_NEW_TARGET,
    )
  }
  let callee = self.member_chain()
  let { offset, count, } = if self.lexer.token == LeftParenthesis {
    self.arguments()
  } else {
    { offset: -1, count: 0, }
  }
  self.program.node(New, callee, offset, count, 0)
}

///|
fn Parser::member_chain(self : Parser) -> Int raise ParseError {
  for expression = self.primary() {
    match self.lexer.token {
      Dot => {
        self.advance()
        continue self.named_member(expression, false)
      }
      LeftBracket => continue self.index_member(expression, false)
      _ => break expression
    }
  }
}

///|
fn Parser::single_arrow(
  self : Parser,
  name_start : Int,
  name_end : Int,
  is_async : Bool,
) -> Int raise ParseError {
  let outer_scope = self.scope
  let saved_generator = self.in_generator
  self.in_generator = false
  let function_scope = self.program.push_scope(
    outer_scope,
    SCOPE_ARROW_PARAMETERS,
  )
  self.scope = function_scope
  guard !self.lexer.newline_before else {
    raise self.syntax_error("newline before arrow")
  }
  let parameter = self.declare_pattern(function_scope, name_start, name_end)
  let name_reference = -1
  self.expect(Arrow)
  let (body, body_flag) = self.arrow_body()
  self.scope = outer_scope
  self.in_generator = saved_generator
  let parameters = self.begin_edges()
  parameters.push(parameter)
  let { offset, count, } = self.program.edge_list(parameters)
  self.program.functions.push({
    name: name_reference,
    parameters_offset: offset,
    parameters_count: count,
    body,
    flags: FUNCTION_ARROW | function_modifier_flags(is_async, false) | body_flag,
  })
  let function_id = self.program.functions.length() - 1
  self.program.node(ArrowExpression, function_id, 0, 0, 0)
}

///|
fn Parser::arrow_body(self : Parser) -> (Int, Int) raise ParseError {
  guard self.lexer.token == LeftBrace else {
    return (self.assignment(), FUNCTION_EXPRESSION_BODY)
  }
  (self.function_body(), 0)
}

///|
fn Parser::parentheses(self : Parser) -> Int raise ParseError {
  let expression = self.parenthesized()
  guard self.lexer.token == Arrow else {
    return self.finish_parenthesized(expression)
  }
  self.finish_arrow(expression, false)
}

///|
fn Parser::template_literal(self : Parser) -> Int raise ParseError {
  guard self.lexer.token == TemplateNoSubstitution else {
    return self.template()
  }
  self.source_literal(TemplateNoSubstitution)
}

///|
fn Parser::template(self : Parser) -> Int raise ParseError {
  let parts = self.begin_edges()
  let mut quasi_count = 0
  while true {
    quasi_count = quasi_count + 1
    parts.push(self.lexer.quasi_start)
    parts.push(self.lexer.quasi_end)
    self.advance()
    let expression = self.expression_with_in()
    parts.push(expression)
    guard self.lexer.token == RightBrace else {
      raise self.syntax_error("template substitution must end with }")
    }
    self.lexer.resume_template()
    guard self.lexer.token != TemplateTail else {
      quasi_count = quasi_count + 1
      parts.push(self.lexer.quasi_start)
      parts.push(self.lexer.quasi_end)
      self.advance()
      break
    }
  }
  let { offset, .. } = self.program.edge_list(parts)
  self.program.node(Template, offset, quasi_count, 0, 0)
}

///|
fn Parser::array_literal(self : Parser) -> Int raise ParseError {
  self.expect(LeftBracket)
  let items = self.begin_edges()
  let mut trailing_comma = false
  while self.lexer.token != RightBracket && self.lexer.token != EndOfFile {
    guard self.lexer.token != Comma else {
      self.advance()
      items.push(-1)
      continue
    }
    if self.lexer.token == Ellipsis {
      self.advance()
      let inner = self.assignment_with_in()
      items.push(self.program.node(Spread, inner, 0, 0, 0))
    } else {
      items.push(self.assignment_with_in())
    }
    guard self.eat(Comma) else { break }
    trailing_comma = self.lexer.token == RightBracket
  }
  self.expect(RightBracket)
  let { offset, count, } = self.program.edge_list(items)
  self.program.node(
    ArrayLiteral,
    offset,
    count,
    0,
    if trailing_comma {
      1
    } else {
      0
    },
  )
}

///|
fn Parser::object_literal(self : Parser) -> Int raise ParseError {
  self.expect(LeftBrace)
  let properties = self.begin_edges()
  let mut trailing_comma = false
  while self.lexer.token != RightBrace && self.lexer.token != EndOfFile {
    properties.push(self.object_property())
    guard self.eat(Comma) else { break }
    trailing_comma = self.lexer.token == RightBrace
  }
  self.expect(RightBrace)
  let { offset, count, } = self.program.edge_list(properties)
  self.program.node(
    ObjectLiteral,
    offset,
    count,
    0,
    if trailing_comma {
      1
    } else {
      0
    },
  )
}

///|
fn Parser::object_property(self : Parser) -> Int raise ParseError {
  guard self.lexer.token != Ellipsis else {
    self.advance()
    let inner = self.assignment_with_in()
    return self.program.node(PatternRest, inner, 0, 0, 0)
  }
  let kind = self.object_method_kind()
  let is_async = self.lexer.token == Identifier &&
    self.lexer.keyword == Keyword::Async &&
    self.property_modifier_follows()
  if is_async {
    self.advance()
  }
  let is_generator = self.eat(Asterisk)
  let is_identifier_key = self.lexer.token == Identifier
  let (key, is_computed) = self.property_key_with_computed()
  let flags = if is_computed { PROPERTY_COMPUTED } else { 0 }
  guard self.lexer.token != LeftParenthesis else {
    let function_id = self.function_tail(-1, is_async, is_generator, false)
    return self.program.node(Method, key, function_id, kind, flags)
  }
  guard !self.eat(Colon) else {
    let value = self.assignment_with_in()
    return self.program.node(Property, key, value, 0, flags)
  }
  let is_shorthand = is_identifier_key && !is_computed
  guard !is_shorthand else { return self.shorthand_property(key) }
  let value = self.assignment_with_in()
  self.program.node(Property, key, value, 0, flags)
}

///|
fn Parser::property_modifier_follows(self : Parser) -> Bool raise ParseError {
  match self.peek_token() {
    Colon | Assignment | Comma | RightBrace | LeftParenthesis => false
    _ => true
  }
}

///|
fn Parser::object_method_kind(self : Parser) -> Int raise ParseError {
  let is_accessor = self.lexer.token == Identifier &&
    (self.lexer.keyword == Keyword::Get || self.lexer.keyword == Keyword::Set) &&
    self.property_modifier_follows()
  guard is_accessor else { return METHOD_ORDINARY }
  let kind = if self.lexer.keyword == Keyword::Get {
    METHOD_GETTER
  } else {
    METHOD_SETTER
  }
  self.advance()
  kind
}

///|
fn Parser::shorthand_property(self : Parser, key : Int) -> Int raise ParseError {
  let reference = self.add_reference(
    self.scope,
    self.program.nodes[key].a,
    self.program.nodes[key].b,
  )
  let binding = self.program.node(Identifier, reference, 0, 0, 0)
  let value = if self.eat(Assignment) {
    let default_value = self.assignment_with_in()
    self.program.cover_defaults += 1
    self.program.node(CoverDefault, binding, default_value, 0, 0)
  } else {
    binding
  }
  self.program.node(Property, key, value, PROPERTY_SHORTHAND, 0)
}

///|
/// Parameter initializers cannot see declarations from the function body.
/// Body var declarations matching a parameter keep its spelling, preserving
/// JavaScript's initialization from the parameter environment.
fn Parser::function_body(self : Parser) -> Int raise ParseError {
  let parameters = self.scope
  let saved_allow_in = self.allow_in
  self.allow_in = true
  let body_scope = self.program.push_scope(parameters, SCOPE_FUNCTION)
  for name, symbol in self.program.scope_members[parameters] {
    self.program.scope_members[body_scope][name] = symbol
  }
  self.scope = body_scope
  self.expect(LeftBrace)
  let statements = self.statements(false)
  self.expect(RightBrace)
  self.scope = parameters
  self.allow_in = saved_allow_in
  let { offset, count, } = self.program.edge_list(statements)
  self.program.node(Block, offset, count, 0, 0)
}

///|
fn Parser::assignment_with_in(self : Parser) -> Int raise ParseError {
  let saved_allow_in = self.allow_in
  self.allow_in = true
  let expression = self.assignment()
  self.allow_in = saved_allow_in
  expression
}

///|
fn Parser::expression_with_in(self : Parser) -> Int raise ParseError {
  let saved_allow_in = self.allow_in
  self.allow_in = true
  let expression = self.expression()
  self.allow_in = saved_allow_in
  expression
}

///|
fn function_modifier_flags(is_async : Bool, is_generator : Bool) -> Int {
  let async_flag = if is_async { FUNCTION_ASYNC } else { 0 }
  let generator_flag = if is_generator { FUNCTION_GENERATOR } else { 0 }
  async_flag | generator_flag
}
