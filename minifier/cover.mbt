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
  let parameter_scope = self.prog.push_scope(
    outer_scope, scope_arrow_parameters,
  )
  self.scope = parameter_scope
  self.expect(LParen)
  let items : Array[Int] = []
  let mut trailing_comma = false
  while self.lx.tok != RParen {
    let item = if self.eat(Ellipsis) {
      let inner = self.parse_assign_in()
      self.prog.node(Spread, inner, 0, 0, 0)
    } else {
      self.parse_assign_in()
    }
    items.push(item)
    if !self.eat(Comma) {
      break
    }
    trailing_comma = self.lx.tok == RParen
  }
  self.expect(RParen)
  { items, outer_scope, parameter_scope, trailing_comma, }
}

///|
fn Parser::finish_arrow(
  self : Parser,
  parameters : Parenthesized,
  is_async : Int,
) -> Int raise ParseError {
  guard !self.lx.newline_before else {
    raise Unexpected("newline before arrow")
  }
  let count = parameters.items.length()
  for index in 0..<count {
    let item = parameters.items[index]
    if self.prog.nodes[item].kind == Spread {
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
  let (offset, count) = self.prog.edge_list(parameters.items)
  let index = self.prog.funcs.length()
  self.prog.funcs.push({
    name: -1,
    params_off: offset,
    params_count: count,
    body,
    flags: fn_arrow + is_async * fn_async + expr_body,
  })
  self.prog.node(ArrowExpr, index, 0, 0, 0)
}

///|
fn Parser::finish_parenthesized(
  self : Parser,
  expression : Parenthesized,
) -> Int raise ParseError {
  let scope = self.prog.scopes[expression.parameter_scope]
  self.prog.scopes[expression.parameter_scope] = {
    ..scope,
    kind: scope_transparent,
  }
  self.scope = expression.outer_scope
  guard !expression.trailing_comma && expression.items.length() > 0 else {
    raise Unexpected("invalid parenthesized expression")
  }
  for item in expression.items {
    guard self.prog.nodes[item].kind != Spread else {
      raise Unexpected("spread requires a parameter or argument list")
    }
  }
  let inner = if expression.items.length() == 1 {
    expression.items[0]
  } else {
    let (offset, count) = self.prog.edge_list(expression.items)
    self.prog.node(Sequence, offset, count, 0, 0)
  }
  self.prog.node(Paren, inner, 0, 0, 0)
}

///|
fn Parser::parse_async_parenthesized(
  self : Parser,
  name_start : Int,
  name_end : Int,
) -> Int raise ParseError {
  let expression = self.parenthesized()
  if self.lx.tok == Arrow {
    return self.finish_arrow(expression, 1)
  }
  let scope = self.prog.scopes[expression.parameter_scope]
  self.prog.scopes[expression.parameter_scope] = {
    ..scope,
    kind: scope_transparent,
  }
  self.scope = expression.outer_scope
  let reference = self.add_ref(self.scope, name_start, name_end)
  let callee = self.prog.node(Ident, reference, 0, 0, 0)
  let (offset, count) = self.prog.edge_list(expression.items)
  self.prog.node(Call, callee, offset, count, 0)
}

///|
/// Reinterpret only binding positions; default-value expressions keep their AST.
fn Parser::cover_pattern(
  self : Parser,
  id : Int,
  declaring : Bool,
) -> Unit raise ParseError {
  let node = self.prog.nodes[id]
  let replacement = match node.kind {
    Ident | PatIdent => {
      let reference = self.prog.refs[node.a]
      if declaring {
        let sym = self.prog.add_symbol(
          reference.name_start,
          reference.name_end,
          self.scope,
        )
        self.prog.refs[node.a] = { ..reference, scope: self.scope, sym, }
      }
      Node::new(if declaring { PatIdent } else { Ident }, node.a, 0, 0, 0)
    }
    ArrayLit | ObjectLit | PatArray | PatObject => {
      for index in 0..<node.b {
        let item = self.prog.edges[node.a + index]
        if item >= 0 {
          let element = self.prog.nodes[item]
          if element.kind == Spread || element.kind == PatRest {
            guard index == node.b - 1 && node.d == 0 else {
              raise Unexpected(
                "rest binding must be last without a trailing comma",
              )
            }
            if (node.kind == ObjectLit || node.kind == PatObject) && declaring {
              let target = self.prog.nodes[element.a].kind
              guard target == Ident || target == PatIdent else {
                raise Unexpected("object rest binding requires an identifier")
              }
            }
          }
          self.cover_pattern(item, declaring)
        }
      }
      let kind = if node.kind == ArrayLit || node.kind == PatArray {
        PatArray
      } else {
        PatObject
      }
      Node::new(kind, node.a, node.b, 0, 0)
    }
    Prop | PatProp => {
      self.cover_pattern(node.b, declaring)
      Node::new(PatProp, node.a, node.b, node.c, node.d)
    }
    Spread | PatRest => {
      let target = self.prog.nodes[node.a].kind
      guard target != Assign && target != PatDefault && target != CoverDefault else {
        raise Unexpected("rest binding cannot have a default")
      }
      self.cover_pattern(node.a, declaring)
      Node::new(PatRest, node.a, 0, 0, 0)
    }
    Assign => {
      guard self.prog.ops[node.a] == Op::Assign else {
        raise Unexpected("invalid default binding")
      }
      self.cover_pattern(node.b, declaring)
      Node::new(PatDefault, node.b, node.c, 0, 0)
    }
    CoverDefault | PatDefault => {
      if node.kind == CoverDefault {
        self.prog.cover_defaults -= 1
      }
      self.cover_pattern(node.a, declaring)
      Node::new(PatDefault, node.a, node.b, 0, 0)
    }
    Member | Index if !declaring => node
    _ => raise Unexpected("invalid binding pattern")
  }
  self.prog.nodes[id] = replacement
}

///|
fn Parser::assignment_pattern(self : Parser, id : Int) -> Unit raise ParseError {
  let kind = self.prog.nodes[id].kind
  if kind == ArrayLit || kind == ObjectLit {
    self.cover_pattern(id, false)
  }
}
