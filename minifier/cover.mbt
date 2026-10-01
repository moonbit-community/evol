///|
#valtype
priv struct Parenthesized {
  items : PendingEdges
  outer_scope : Int
  parameter_scope : Int
  trailing_comma : Bool
}

///|
/// Parse the common prefix of an expression, call and arrow parameter list once.
/// The provisional scope is retained for arrows and collapsed otherwise.
fn Parser::parenthesized(self : Parser) -> Parenthesized raise ParseError {
  let outer_scope = self.scope
  let parameter_scope = self.program.push_scope(
    outer_scope, scope_arrow_parameters,
  )
  self.scope = parameter_scope
  self.expect(LeftParenthesis)
  let items = self.begin_edges()
  let mut trailing_comma = false
  while self.lexer.token != RightParenthesis {
    let item = if self.eat(Ellipsis) {
      let inner = self.assignment_with_in()
      self.program.node(Spread, inner, 0, 0, 0)
    } else {
      self.assignment_with_in()
    }
    items.push(item)
    if !self.eat(Comma) {
      break
    }
    trailing_comma = self.lexer.token == RightParenthesis
  }
  self.expect(RightParenthesis)
  { items, outer_scope, parameter_scope, trailing_comma, }
}

///|
fn Parser::finish_arrow(
  self : Parser,
  parameters : Parenthesized,
  is_async : Bool,
) -> Int raise ParseError {
  guard !self.lexer.newline_before else {
    raise self.syntax_error("newline before arrow")
  }
  let count = parameters.items.length()
  for index in 0..<count {
    let item = parameters.items[index]
    if self.program.nodes[item].kind == Spread {
      guard index == count - 1 && !parameters.trailing_comma else {
        raise self.syntax_error("rest parameter must be last")
      }
    }
    self.cover_pattern(item, true)
  }
  let saved_generator = self.in_generator
  self.in_generator = false
  self.expect(Arrow)
  let (body, body_flag) = self.arrow_body()
  self.in_generator = saved_generator
  self.scope = parameters.outer_scope
  let { offset, count, } = self.program.edge_list(parameters.items)
  let index = self.program.functions.length()
  self.program.functions.push({
    name: -1,
    parameters_offset: offset,
    parameters_count: count,
    body,
    flags: FUNCTION_ARROW | function_modifier_flags(is_async, false) | body_flag,
  })
  self.program.node(ArrowExpression, index, 0, 0, 0)
}

///|
fn Parser::finish_parenthesized(
  self : Parser,
  expression : Parenthesized,
) -> Int raise ParseError {
  self.collapse_parenthesized_scope(expression)
  guard !expression.trailing_comma && expression.items.length() > 0 else {
    raise self.syntax_error("invalid parenthesized expression")
  }
  for item in expression.items.view() {
    guard self.program.nodes[item].kind != Spread else {
      raise self.syntax_error("spread requires a parameter or argument list")
    }
  }
  let inner = if expression.items.length() == 1 {
    let inner = expression.items[0]
    expression.items.release()
    inner
  } else {
    let { offset, count, } = self.program.edge_list(expression.items)
    self.program.node(Sequence, offset, count, 0, 0)
  }
  self.program.node(Parenthesized, inner, 0, 0, 0)
}

///|
fn Parser::async_parentheses(
  self : Parser,
  name_start : Int,
  name_end : Int,
) -> Int raise ParseError {
  let expression = self.parenthesized()
  if self.lexer.token == Arrow {
    return self.finish_arrow(expression, true)
  }
  self.collapse_parenthesized_scope(expression)
  let reference = self.add_reference(self.scope, name_start, name_end)
  let callee = self.program.node(Identifier, reference, 0, 0, 0)
  let { offset, count, } = self.program.edge_list(expression.items)
  self.program.node(Call, callee, offset, count, 0)
}

///|
/// Reinterpret only binding positions; default-value expressions keep their AST.
fn Parser::cover_pattern(
  self : Parser,
  id : Int,
  declaring : Bool,
) -> Unit raise ParseError {
  let node = self.program.nodes[id]
  let replacement = match node.kind {
    Identifier | PatternIdentifier => self.cover_identifier(node, declaring)
    ArrayLiteral | ObjectLiteral | PatternArray | PatternObject =>
      self.cover_collection(node, declaring)
    Property | PatternProperty => {
      self.cover_pattern(node.b, declaring)
      Node::new(PatternProperty, node.a, node.b, node.c, node.d)
    }
    Spread | PatternRest => self.cover_rest(node, declaring)
    Assignment => {
      guard self.program.operators[node.a] == Operator::Assignment else {
        raise self.syntax_error("invalid default binding")
      }
      self.cover_pattern(node.b, declaring)
      Node::new(PatternDefault, node.b, node.c, 0, 0)
    }
    CoverDefault | PatternDefault => {
      if node.kind == CoverDefault {
        self.program.cover_defaults -= 1
      }
      self.cover_pattern(node.a, declaring)
      Node::new(PatternDefault, node.a, node.b, 0, 0)
    }
    Member | Index if !declaring => node
    _ => raise self.syntax_error("invalid binding pattern")
  }
  self.program.nodes[id] = replacement
}

///|
fn Parser::cover_identifier(
  self : Parser,
  node : Node,
  declaring : Bool,
) -> Node raise ParseError {
  let reference = self.program.references[node.a]
  if declaring {
    let symbol = self.program.add_symbol(
      reference.name_start,
      reference.name_end,
      self.scope,
    )
    self.program.references[node.a] = {
      ..reference,
      scope: self.scope,
      symbol,
    }
  }
  let kind = if declaring { PatternIdentifier } else { Identifier }
  Node::new(kind, node.a, 0, 0, 0)
}

///|
fn Parser::cover_collection(
  self : Parser,
  node : Node,
  declaring : Bool,
) -> Node raise ParseError {
  let is_object = node.kind == ObjectLiteral || node.kind == PatternObject
  for index in 0..<node.b {
    let item = self.program.edges[node.a + index]
    guard item >= 0 else { continue }
    let element = self.program.nodes[item]
    if element.kind == Spread || element.kind == PatternRest {
      let is_last_without_comma = index == node.b - 1 && node.d == 0
      guard is_last_without_comma else {
        raise self.syntax_error(
          "rest binding must be last without a trailing comma",
        )
      }
      if is_object && declaring {
        self.require_object_rest_identifier(element.a)
      }
    }
    self.cover_pattern(item, declaring)
  }
  let kind = if is_object { PatternObject } else { PatternArray }
  Node::new(kind, node.a, node.b, 0, 0)
}

///|
fn Parser::require_object_rest_identifier(
  self : Parser,
  target : Int,
) -> Unit raise ParseError {
  let kind = self.program.nodes[target].kind
  guard kind == Identifier || kind == PatternIdentifier else {
    raise self.syntax_error("object rest binding requires an identifier")
  }
}

///|
fn Parser::cover_rest(
  self : Parser,
  node : Node,
  declaring : Bool,
) -> Node raise ParseError {
  let target = self.program.nodes[node.a].kind
  let has_default = target == Assignment ||
    target == PatternDefault ||
    target == CoverDefault
  guard !has_default else {
    raise self.syntax_error("rest binding cannot have a default")
  }
  self.cover_pattern(node.a, declaring)
  Node::new(PatternRest, node.a, 0, 0, 0)
}

///|
fn Parser::assignment_pattern(self : Parser, id : Int) -> Unit raise ParseError {
  let kind = self.program.nodes[id].kind
  if kind == ArrayLiteral || kind == ObjectLiteral {
    self.cover_pattern(id, false)
  }
}

///|
fn Parser::collapse_parenthesized_scope(
  self : Parser,
  expression : Parenthesized,
) -> Unit {
  let scope = self.program.scopes[expression.parameter_scope]
  self.program.scopes[expression.parameter_scope] = {
    ..scope,
    kind: scope_transparent,
  }
  self.scope = expression.outer_scope
}
