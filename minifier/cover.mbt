///|
#valtype
priv struct Parenthesized {
  items : Array[Int]
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
  let items : Array[Int] = []
  let mut trailing_comma = false
  while self.lexer.token != RightParenthesis {
    let item = if self.eat(Ellipsis) {
      let inner = self.parse_assignment_allowing_in()
      self.program.node(Spread, inner, 0, 0, 0)
    } else {
      self.parse_assignment_allowing_in()
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
  is_async : Int,
) -> Int raise ParseError {
  guard !self.lexer.newline_before else {
    raise Unexpected("newline before arrow")
  }
  let count = parameters.items.length()
  for index in 0..<count {
    let item = parameters.items[index]
    if self.program.nodes[item].kind == Spread {
      guard index == count - 1 && !parameters.trailing_comma else {
        raise Unexpected("rest parameter must be last")
      }
    }
    self.cover_pattern(item, true)
  }
  let saved_generator = self.in_generator
  self.in_generator = false
  self.expect(Arrow)
  let (body, expr_body) = self.parse_arrow_body()
  self.in_generator = saved_generator
  self.scope = parameters.outer_scope
  let (offset, count) = self.program.edge_list(parameters.items)
  let index = self.program.functions.length()
  self.program.functions.push({
    name: -1,
    parameters_offset: offset,
    parameters_count: count,
    body,
    flags: function_flag_arrow + is_async * function_flag_async + expr_body,
  })
  self.program.node(ArrowExpression, index, 0, 0, 0)
}

///|
fn Parser::finish_parenthesized(
  self : Parser,
  expression : Parenthesized,
) -> Int raise ParseError {
  let scope = self.program.scopes[expression.parameter_scope]
  self.program.scopes[expression.parameter_scope] = {
    ..scope,
    kind: scope_transparent,
  }
  self.scope = expression.outer_scope
  guard !expression.trailing_comma && expression.items.length() > 0 else {
    raise Unexpected("invalid parenthesized expression")
  }
  for item in expression.items {
    guard self.program.nodes[item].kind != Spread else {
      raise Unexpected("spread requires a parameter or argument list")
    }
  }
  let inner = if expression.items.length() == 1 {
    expression.items[0]
  } else {
    let (offset, count) = self.program.edge_list(expression.items)
    self.program.node(Sequence, offset, count, 0, 0)
  }
  self.program.node(Parenthesized, inner, 0, 0, 0)
}

///|
fn Parser::parse_async_parenthesized(
  self : Parser,
  name_start : Int,
  name_end : Int,
) -> Int raise ParseError {
  let expression = self.parenthesized()
  if self.lexer.token == Arrow {
    return self.finish_arrow(expression, 1)
  }
  let scope = self.program.scopes[expression.parameter_scope]
  self.program.scopes[expression.parameter_scope] = {
    ..scope,
    kind: scope_transparent,
  }
  self.scope = expression.outer_scope
  let reference = self.add_reference(self.scope, name_start, name_end)
  let callee = self.program.node(Identifier, reference, 0, 0, 0)
  let (offset, count) = self.program.edge_list(expression.items)
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
    Identifier | PatternIdentifier => {
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
      Node::new(
        if declaring {
          PatternIdentifier
        } else {
          Identifier
        },
        node.a,
        0,
        0,
        0,
      )
    }
    ArrayLiteral | ObjectLiteral | PatternArray | PatternObject => {
      for index in 0..<node.b {
        let item = self.program.edges[node.a + index]
        if item >= 0 {
          let element = self.program.nodes[item]
          if element.kind == Spread || element.kind == PatternRest {
            guard index == node.b - 1 && node.d == 0 else {
              raise Unexpected(
                "rest binding must be last without a trailing comma",
              )
            }
            if (node.kind == ObjectLiteral || node.kind == PatternObject) &&
              declaring {
              let target = self.program.nodes[element.a].kind
              guard target == Identifier || target == PatternIdentifier else {
                raise Unexpected("object rest binding requires an identifier")
              }
            }
          }
          self.cover_pattern(item, declaring)
        }
      }
      let kind = if node.kind == ArrayLiteral || node.kind == PatternArray {
        PatternArray
      } else {
        PatternObject
      }
      Node::new(kind, node.a, node.b, 0, 0)
    }
    Property | PatternProperty => {
      self.cover_pattern(node.b, declaring)
      Node::new(PatternProperty, node.a, node.b, node.c, node.d)
    }
    Spread | PatternRest => {
      let target = self.program.nodes[node.a].kind
      guard target != Assignment &&
        target != PatternDefault &&
        target != CoverDefault else {
        raise Unexpected("rest binding cannot have a default")
      }
      self.cover_pattern(node.a, declaring)
      Node::new(PatternRest, node.a, 0, 0, 0)
    }
    Assignment => {
      guard self.program.operators[node.a] == Operator::Assignment else {
        raise Unexpected("invalid default binding")
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
    _ => raise Unexpected("invalid binding pattern")
  }
  self.program.nodes[id] = replacement
}

///|
fn Parser::assignment_pattern(self : Parser, id : Int) -> Unit raise ParseError {
  let kind = self.program.nodes[id].kind
  if kind == ArrayLiteral || kind == ObjectLiteral {
    self.cover_pattern(id, false)
  }
}
