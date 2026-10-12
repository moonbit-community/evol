///|
fn Program::compact_branch(self : Program, id : Int) -> Unit {
  let branch = self.nodes[id]
  guard branch.kind is If else { return }
  let consequent = self.nodes[self.single_statement(branch.b)]
  if branch.c < 0 && consequent.kind is If && consequent.c < 0 {
    let condition = self.slot_logical(LogicalAnd, branch.a, consequent.a)
    self.nodes[id] = Node::new(If, condition, consequent.b, -1, 0)
    return
  }
  guard branch.c >= 0 else { return }
  let alternative = self.nodes[self.single_statement(branch.c)]
  guard consequent.kind == alternative.kind &&
    consequent.kind is Return &&
    consequent.a >= 0 &&
    alternative.a >= 0 else {
    return
  }
  let value = self.compact_choice(branch.a, consequent.a, alternative.a)
  self.nodes[id] = Node::new(consequent.kind, value, 0, 0, 0)
}

///|
/// The only false path exits this exact loop. Keep the original true-branch
/// block so its lexical declarations still receive a fresh iteration scope.
fn Program::compact_loop_guard(self : Program, id : Int) -> Unit {
  let iteration = self.nodes[id]
  guard iteration.kind is For &&
    iteration.a < 0 &&
    iteration.b < 0 &&
    iteration.c < 0 else {
    return
  }
  let branch = self.nodes[self.single_statement(iteration.d)]
  guard branch.kind is If && branch.c >= 0 else { return }
  let alternative = self.nodes[self.single_statement(branch.c)]
  guard alternative.kind is Break && alternative.a < 0 else { return }
  // A bare Annex B function belongs to its if clause. Making it a direct
  // loop body would change both its grammar and declaration semantics.
  let consequent = self.nodes[branch.b]
  match consequent.kind {
    FunctionDeclaration | ClassDeclaration => return
    VariableDeclaration if consequent.a != DECLARATION_VAR => return
    _ => ()
  }
  self.nodes[id] = { ..iteration, b: branch.a, d: branch.b, }
}

///|
fn Program::compact_statement_list(
  self : Program,
  offset : Int,
  count : Int,
  bindings : ValueBindings,
) -> Int {
  // Work backwards so a chain of adjacent single-use values can collapse
  // without rescanning the statement list to a fixed point.
  for index = count - 2; index >= 0; index = index - 1 {
    let statement = self.edges[offset + index]
    let next = self.edges[offset + index + 1]
    let branch = self.nodes[statement]
    let following = self.nodes[next]
    if branch.kind is ExpressionStatement &&
      following.kind is (Return | Throw | ExpressionStatement) &&
      following.a >= 0 &&
      !(self.nodes[branch.a].kind is StringLiteral) &&
      !(following.kind is ExpressionStatement &&
      self.nodes[following.a].kind is StringLiteral) {
      let value = self.sequence_pair(branch.a, following.a)
      self.nodes[next] = { ..following, a: value, }
      self.nodes[statement] = Node::new(Elided, 0, 0, 0, 0)
      self.edges[offset + index] = next
      self.edges[offset + index + 1] = statement
      continue
    }
    if branch.kind is If && self.compact_guarded_unit_return(statement, next) {
      continue
    }
    if branch.kind is If &&
      branch.c >= 0 &&
      following.kind is Return &&
      following.a >= 0 &&
      self.compact_value_branch_return(statement, next) {
      self.nodes[statement] = Node::new(Elided, 0, 0, 0, 0)
      self.edges[offset + index] = next
      self.edges[offset + index + 1] = statement
      continue
    }
    if branch.kind is If && branch.c < 0 && self.nodes[next].kind is Return {
      let consequent = self.nodes[self.single_statement(branch.b)]
      let alternative = self.nodes[next]
      if consequent.kind is Return && consequent.a >= 0 && alternative.a >= 0 {
        let value = self.compact_choice(branch.a, consequent.a, alternative.a)
        self.nodes[next] = Node::new(Return, value, 0, 0, 0)
        self.nodes[statement] = Node::new(Elided, 0, 0, 0, 0)
        self.edges[offset + index] = next
        self.edges[offset + index + 1] = statement
        continue
      }
    }
    if self.inline_adjacent_value(statement, next, bindings) {
      self.nodes[statement] = Node::new(Elided, 0, 0, 0, 0)
      self.edges[offset + index] = next
      self.edges[offset + index + 1] = statement
    }
  }
  self.live_statements(offset, count)
}

///|
fn Program::live_statements(self : Program, offset : Int, count : Int) -> Int {
  let mut source_directives = true
  let mut output_directives = true
  for read = 0, kept = 0 {
    guard read < count else { break kept }
    let statement = self.edges[offset + read]
    let node = self.nodes[statement]
    if node.kind is Elided {
      source_directives = false
      continue read + 1, kept
    }
    let is_directive = node.kind is ExpressionStatement &&
      self.nodes[node.a].kind is StringLiteral
    if !is_directive {
      source_directives = false
      output_directives = false
    } else if !source_directives && output_directives {
      let value = self.node(Parenthesized, node.a, 0, 0, 0)
      self.nodes[statement] = { ..node, a: value, }
      output_directives = false
    }
    self.edges[offset + kept] = statement
    continue read + 1, kept + 1
  }
}

///|
fn Program::sequence_pair(self : Program, first : Int, second : Int) -> Int {
  let offset = self.edges.length()
  self.edges.push(first)
  self.edges.push(second)
  self.node(Sequence, offset, 2, 0, 0)
}

///|
fn Program::inline_adjacent_value(
  self : Program,
  declaration_id : Int,
  consumer_id : Int,
  bindings : ValueBindings,
) -> Bool {
  let declaration = self.nodes[declaration_id]
  guard declaration.kind is VariableDeclaration &&
    declaration.a != DECLARATION_VAR &&
    declaration.c == 1 else {
    return false
  }
  let binding = self.nodes[self.edges[declaration.b]]
  let pattern = self.nodes[binding.a]
  guard pattern.kind is PatternIdentifier && binding.b >= 0 else {
    return false
  }
  let symbol = self.references[pattern.a].symbol
  let entry = self.symbols[symbol]
  guard (entry.scope > 0 || bindings.rename_top_level) &&
    entry.flags == 0 &&
    entry.link < 0 &&
    entry.use_count == 2 &&
    !self.preserved_names.contains(entry.name) &&
    !bindings.has_writes[symbol] else {
    return false
  }
  // Function-name changes are allowed. Class inferred names remain preserved.
  let initializer = self.nodes[self.unwrapped_expression(binding.b)]
  guard !(initializer.kind is ClassExpression) else { return false }
  let expression = self.first_statement_expression(consumer_id)
  let movement = ValueMovement::new(bindings, symbol, declaration_id, binding.b)
  let reference = self.leading_value_read(expression, movement)
  guard reference >= 0 else { return false }
  // An expression replaces a primary identifier. Keep its grouping in every
  // surrounding operator context, including member access and exponentiation.
  self.nodes[reference] = if movement.detach_receiver &&
    initializer.kind is (Identifier | Member | Index) {
    let zero = self.node(ZeroLiteral, 0, 0, 0, 0)
    let value = self.sequence_pair(zero, binding.b)
    Node::new(Parenthesized, value, 0, 0, 0)
  } else if reference == expression &&
    self.nodes[consumer_id].kind is ExpressionStatement &&
    initializer.kind is StringLiteral {
    Node::new(Parenthesized, binding.b, 0, 0, 0)
  } else if self.is_primary_value(binding.b) ||
    (
      reference == expression &&
      self.nodes[consumer_id].kind is (Return | Throw | If | Switch)
    ) {
    self.nodes[binding.b]
  } else {
    Node::new(Parenthesized, binding.b, 0, 0, 0)
  }
  self.symbols[symbol] = { ..entry, flags: entry.flags | SYMBOL_ELIDED, }
  true
}

///|
fn Program::is_primary_value(self : Program, id : Int) -> Bool {
  let node = self.nodes[id]
  match node.kind {
    Identifier
    | StringLiteral
    | ArrayLiteral
    | RegexLiteral
    | TemplateNoSubstitution
    | Parenthesized => true
    Member | Index | Call => !self.has_optional_chain(id)
    New => node.b >= 0
    _ => false
  }
}

///|
fn Program::has_optional_chain(self : Program, id : Int) -> Bool {
  guard id >= 0 else { return false }
  let node = self.nodes[id]
  match node.kind {
    Member | Index | Call =>
      node.d == CHAIN_OPTIONAL || self.has_optional_chain(node.a)
    _ => false
  }
}

///|
fn Program::first_statement_expression(self : Program, id : Int) -> Int {
  let node = self.nodes[id]
  match node.kind {
    Return | Throw | ExpressionStatement | If | Switch => node.a
    VariableDeclaration if node.c > 0 => self.nodes[self.edges[node.b]].b
    _ => -1
  }
}

///|
/// Find a read evaluated before any other potentially observable operation.
/// Do not cross calls, coercions, short-circuit arms, scopes or loop boundaries.
fn Program::leading_value_read(
  self : Program,
  id : Int,
  movement : ValueMovement,
) -> Int {
  guard id >= 0 else { return -1 }
  let node = self.nodes[id]
  match node.kind {
    Identifier =>
      if self.references[node.a].symbol == movement.symbol {
        id
      } else {
        -1
      }
    Parenthesized | EvaluatedValue | Member | Conditional =>
      self.leading_value_read(node.a, movement)
    Index => {
      let object = self.leading_value_read(node.a, movement)
      guard object < 0 else { return object }
      guard node.d != CHAIN_OPTIONAL && movement.can_cross_read(node.a) else {
        return -1
      }
      self.leading_value_read(node.b, movement)
    }
    Binary => {
      let left = self.leading_value_read(node.b, movement)
      guard left < 0 else { return left }
      guard movement.can_cross_read(node.b) else { return -1 }
      self.leading_value_read(node.c, movement)
    }
    Logical => self.leading_value_read(node.b, movement)
    Unary if !(self.operators[node.a] is (TypeOf | Delete)) =>
      self.leading_value_read(node.b, movement)
    Assignment if self.operators[node.a] is Operator::Assignment &&
      self.stable_assignment_target(node.b, movement) =>
      self.leading_value_read(node.c, movement)
    Call | New => {
      let callee = self.unwrapped_expression(node.a)
      let target = self.nodes[callee]
      if target.kind is Identifier &&
        self.references[target.a].symbol == movement.symbol {
        movement.detach_receiver = node.kind is Call
        return callee
      }
      if target.kind is (Member | Index | EvaluatedValue) {
        let reference = self.leading_value_read(callee, movement)
        guard reference < 0 else { return reference }
      }
      guard node.b >= 0 && movement.can_cross_read(node.a) else { return -1 }
      self.leading_list_read(node.b, node.c, movement)
    }
    ArrayLiteral | Sequence => self.leading_list_read(node.a, node.b, movement)
    ObjectLiteral => {
      for index in 0..<node.b {
        let property = self.nodes[self.edges[node.a + index]]
        guard property.kind is Property &&
          property.d != PROPERTY_COMPUTED &&
          property.c != PROPERTY_SHORTHAND else {
          return -1
        }
        let reference = self.leading_value_read(property.b, movement)
        guard reference < 0 else { return reference }
        guard movement.can_cross_read(property.b) else { return -1 }
      }
      -1
    }
    _ => -1
  }
}

///|
fn Program::stable_assignment_target(
  self : Program,
  id : Int,
  movement : ValueMovement,
) -> Bool {
  let node = self.nodes[self.unwrapped_expression(id)]
  match node.kind {
    Identifier => self.references[node.a].symbol >= 0
    Member if node.a >= 0 => movement.can_cross_read(node.a)
    _ => false
  }
}

///|
fn Program::leading_list_read(
  self : Program,
  offset : Int,
  count : Int,
  movement : ValueMovement,
) -> Int {
  for index in 0..<count {
    let expression = self.edges[offset + index]
    guard expression >= 0 else { continue }
    let reference = self.leading_value_read(expression, movement)
    guard reference < 0 else { return reference }
    guard movement.can_cross_read(expression) else { return -1 }
  }
  -1
}
