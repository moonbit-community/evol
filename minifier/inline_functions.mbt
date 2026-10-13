///|
/// Move a single-use function value only within the execution frame that
/// originally creates it. A nested function or loop could allocate it again.
fn ValueBindings::inline_function_values(self : ValueBindings) -> Unit {
  let program = self.program
  let scopes : @hashset.HashSet[Int32] = @hashset.HashSet([])
  for id in self.function_value_declarations {
    let declaration = program.nodes[id]
    guard declaration.kind is FunctionDeclaration else { continue }
    let name = program.functions[declaration.a].name
    guard name >= 0 else { continue }
    let symbol = program.references[name].symbol
    let entry = program.symbols[symbol]
    if self.functions[symbol] &&
      !self.has_writes[symbol] &&
      entry.flags == 0 &&
      entry.link < 0 &&
      entry.use_count == 2 &&
      !program.preserved_names.contains(entry.name) {
      scopes.add(entry.scope)
    }
  }
  guard scopes.length() > 0 else { return }
  if scopes.contains(0) {
    self.inline_function_body(program.root)
  }
  // Function bodies record their execution scope. Most contain no candidate
  // declarations, so their expressions need no additional traversal.
  for function in program.functions {
    let body = program.nodes[function.body]
    if body.kind is Block && scopes.contains(body.c) {
      self.inline_function_body(function.body)
    }
  }
}

///|
fn ValueBindings::inline_function_body(
  self : ValueBindings,
  id : Int32,
) -> Unit {
  let program = self.program
  let body = program.nodes[id]
  for index in 0..<body.b {
    self.inline_function_statement(program.edges[body.a + index])
  }
  // Removing a declaration must not promote a later string to a directive.
  let count = program.live_statements(body.a, body.b)
  program.nodes[id] = { ..body, b: count, }
}

///|
fn ValueBindings::inline_function_statement(
  self : ValueBindings,
  id : Int32,
) -> Unit {
  guard id >= 0 else { return }
  let program = self.program
  let statement = program.nodes[id]
  match statement.kind {
    VariableDeclaration =>
      for index in 0..<statement.c {
        let binding = program.nodes[program.edges[statement.b + index]]
        self.inline_function_expression(binding.b, false)
      }
    ExpressionStatement | Return | Throw | Switch =>
      self.inline_function_expression(statement.a, false)
    If => {
      self.inline_function_expression(statement.a, false)
      self.inline_function_statement(statement.b)
      self.inline_function_statement(statement.c)
    }
    _ => ()
  }
}

///|
fn ValueBindings::inline_function_expression(
  self : ValueBindings,
  id : Int32,
  primary_context : Bool,
) -> Unit {
  guard id >= 0 else { return }
  let program = self.program
  let node = program.nodes[id]
  match node.kind {
    Identifier => {
      let reference = program.references[node.a]
      let symbol = reference.symbol
      guard symbol >= 0 && self.functions[symbol] && !self.has_writes[symbol] else {
        return
      }
      let entry = program.symbols[symbol]
      guard entry.scope == reference.scope &&
        entry.flags == 0 &&
        entry.link < 0 &&
        entry.use_count == 2 &&
        !program.preserved_names.contains(entry.name) else {
        return
      }
      let declaration_id = self.declarations[symbol]
      guard declaration_id >= 0 else { return }
      let declaration = program.nodes[declaration_id]
      guard declaration.kind is FunctionDeclaration else { return }
      let function = program.functions[declaration.a]
      program.functions[declaration.a] = { ..function, name: -1, }
      program.nodes[id] = if primary_context {
        let value = program.node(FunctionExpression, declaration.a, 0, 0, 0)
        Node::new(Parenthesized, value, 0, 0, 0)
      } else {
        Node::new(FunctionExpression, declaration.a, 0, 0, 0)
      }
      program.symbols[symbol] = { ..entry, flags: entry.flags | SYMBOL_ELIDED, }
      program.nodes[declaration_id] = Node::new(Elided, 0, 0, 0, 0)
    }
    Parenthesized | EvaluatedValue | Spread =>
      self.inline_function_expression(node.a, false)
    Member | Index => self.inline_function_expression(node.a, true)
    Unary if !(program.operators[node.a] is Delete) =>
      self.inline_function_expression(node.b, false)
    Binary | Logical => {
      self.inline_function_expression(node.b, false)
      self.inline_function_expression(node.c, false)
    }
    Assignment => self.inline_function_expression(node.c, false)
    Conditional => {
      self.inline_function_expression(node.a, false)
      self.inline_function_expression(node.b, false)
      self.inline_function_expression(node.c, false)
    }
    Call | New => {
      self.inline_function_expression(node.a, true)
      if node.b >= 0 {
        self.inline_function_list(node.b, node.c)
      }
    }
    TaggedTemplate => self.inline_function_expression(node.a, true)
    ArrayLiteral | Sequence => self.inline_function_list(node.a, node.b)
    ObjectLiteral =>
      for index in 0..<node.b {
        let property = program.nodes[program.edges[node.a + index]]
        guard property.kind is Property &&
          property.d != PROPERTY_COMPUTED &&
          property.c != PROPERTY_SHORTHAND else {
          continue
        }
        self.inline_function_expression(property.b, false)
      }
    _ => ()
  }
}

///|
fn ValueBindings::inline_function_list(
  self : ValueBindings,
  offset : Int32,
  count : Int32,
) -> Unit {
  for index in 0..<count {
    self.inline_function_expression(self.program.edges[offset + index], false)
  }
}

///|
/// Inline only bodies that cannot observe the call frame: literal constructors
/// and parameter identity. No calls, accessors, coercions, defaults or rest.
fn ValueBindings::inline_trivial_call(
  self : ValueBindings,
  id : Int32,
  rename_top_level : Bool,
) -> Unit {
  let program = self.program
  let call = program.nodes[id]
  guard call.c <= 1 else { return }
  let callee = program.nodes[program.unwrapped_expression(call.a)]
  guard callee.kind is Identifier else { return }
  let reference = program.references[callee.a]
  let symbol = reference.symbol
  guard symbol >= 0 && !self.has_writes[symbol] else { return }
  guard program.symbols[symbol].scope > 0 || rename_top_level else { return }
  let declaration_id = self.declarations[symbol]
  guard declaration_id >= 0 else { return }
  let declaration = program.nodes[declaration_id]
  let function_id = match declaration.kind {
    FunctionDeclaration if self.functions[symbol] => declaration.a
    VariableDeclaration if declaration.a == DECLARATION_CONST &&
      declaration.c == 1 &&
      self.stable_prefix(call.a, id, reference.scope) => {
      let binding = program.nodes[program.edges[declaration.b]]
      guard binding.b >= 0 else { return }
      let value = program.nodes[program.unwrapped_expression(binding.b)]
      guard value.kind is (ArrowExpression | FunctionExpression) else { return }
      value.a
    }
    _ => return
  }
  let function = program.functions[function_id]
  guard (function.flags & (FUNCTION_ASYNC | FUNCTION_GENERATOR)) == 0 else {
    return
  }
  for index in 0..<function.parameters_count {
    guard program.nodes[program.edges[function.parameters_offset + index]].kind
      is PatternIdentifier else {
      return
    }
  }
  let result = if (function.flags & FUNCTION_EXPRESSION_BODY) != 0 {
    function.body
  } else {
    let body = program.nodes[function.body]
    guard body.kind is Block && body.b == 1 else { return }
    let statement = program.nodes[program.edges[body.a]]
    guard statement.kind is Return && statement.a >= 0 else { return }
    statement.a
  }
  let value = program.nodes[program.unwrapped_expression(result)]
  // A sloppy call used as a write target is parsed successfully and throws
  // only when evaluated. Replacing that Call can turn it into a syntax error.
  if self.call_write_targets.length() > 0 {
    for target in self.call_write_targets {
      guard target != id else { return }
    }
  }
  let replacement = match value.kind {
    NumberLiteral | BooleanLiteral | NullLiteral => {
      guard call.c == 0 else { return }
      value
    }
    StringLiteral => {
      guard call.c == 0 else { return }
      // The call could have been the statement that ended a directive prefix.
      // Give it its own literal node instead of sharing parenthesized nodes
      // from the function body with later transformations at the call site.
      let literal = program.node(
        StringLiteral,
        value.a,
        value.b,
        value.c,
        value.d,
      )
      Node::new(Parenthesized, literal, 0, 0, 0)
    }
    ArrayLiteral if value.b == 0 => {
      guard call.c == 0 else { return }
      value
    }
    Identifier => {
      guard function.parameters_count > 0 && call.c == 1 else { return }
      let parameter = program.identifier_symbol(
        program.edges[function.parameters_offset],
      )
      guard parameter >= 0 && program.references[value.a].symbol == parameter else {
        return
      }
      // Sloppy functions can repeat a parameter name. A later occurrence
      // overwrites the first argument, even when that later argument is absent.
      for index in 1..<function.parameters_count {
        if program.identifier_symbol(
            program.edges[function.parameters_offset + index],
          ) ==
          parameter {
          return
        }
      }
      let argument = program.edges[call.b]
      guard !(program.nodes[argument].kind is Spread) else { return }
      Node::new(EvaluatedValue, argument, 0, 0, 0)
    }
    _ => return
  }
  program.nodes[id] = replacement
  let entry = program.symbols[symbol]
  let remaining = entry.use_count - 1
  let flags = if remaining == 1 {
    entry.flags | SYMBOL_ELIDED
  } else {
    entry.flags
  }
  program.symbols[symbol] = { ..entry, use_count: remaining, flags, }
  if remaining == 1 {
    program.nodes[declaration_id] = Node::new(Elided, 0, 0, 0, 0)
  }
}
