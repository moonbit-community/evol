///|
fn Program::compact_patterns(self : Program, rename_top_level : Bool) -> Unit {
  guard !self.has_dynamic_scope else { return }
  let calls = InternalCalls::new(self, rename_top_level)
  let bindings = ValueBindings::new(self, rename_top_level)
  bindings.inline_aliases(rename_top_level)
  bindings.inline_data_values(rename_top_level)
  // Parser arena order places statements after their children. New expression
  // nodes are already simplified when created and need no second traversal.
  let parsed_nodes = self.nodes.length()
  for id in 0..<parsed_nodes {
    let node = self.nodes[id]
    match node.kind {
      Identifier | VariableDeclaration => calls.collect_node(id, node)
      Call => {
        calls.collect_node(id, node)
        bindings.inline_trivial_call(id, rename_top_level)
      }
      FunctionDeclaration => {
        calls.collect_node(id, node)
        self.remove_final_unit_return(node.a)
      }
      FunctionExpression | ArrowExpression =>
        self.remove_final_unit_return(node.a)
      Method => self.remove_final_unit_return(node.b)
      Return | Binary | Unary | Conditional => self.compact_expression(id)
      Parenthesized => self.compact_parenthesized(id)
      Block => {
        let live = self.live_statements(node.a, node.b)
        let slots = self.compact_result_slots(node.a, live, bindings)
        let count = self.compact_statement_list(node.a, slots, bindings)
        self.nodes[id] = { ..node, b: count, }
      }
      SwitchCase => {
        let live = self.live_statements(node.b, node.c)
        let slots = self.compact_result_slots(node.b, live, bindings)
        let count = self.compact_statement_list(node.b, slots, bindings)
        self.nodes[id] = { ..node, c: count, }
      }
      If => {
        self.compact_branch(id)
        self.compact_unit_branch(id)
      }
      Labeled => self.compact_label_exit(id)
      Switch => self.compact_switch_returns(id)
      While => {
        self.compact_expression(id)
        self.remove_tail_continue(node.b)
        self.compact_loop_guard(id)
      }
      For => {
        self.remove_tail_continue(node.d)
        self.compact_loop_guard(id)
      }
      _ => ()
    }
  }
  calls.finish()
  bindings.inline_function_values()
}

///|
fn Program::identifier_symbol(self : Program, id : Int) -> Int {
  guard id >= 0 else { return -1 }
  let node = self.nodes[self.unwrapped_expression(id)]
  guard node.kind is (Identifier | PatternIdentifier) else { return -1 }
  self.references[node.a].symbol
}

///|
fn Program::result_slot(self : Program, declaration : Int) -> Int {
  let node = self.nodes[declaration]
  guard node.kind is VariableDeclaration &&
    node.a == DECLARATION_LET &&
    node.c == 1 else {
    return -1
  }
  let binding = self.nodes[self.edges[node.b]]
  guard binding.b < 0 else { return -1 }
  let symbol = self.identifier_symbol(binding.a)
  guard symbol >= 0 else { return -1 }
  let entry = self.symbols[symbol]
  // Top-level bindings may be externally observed in --top mode. Linked or
  // preserved bindings also have identities beyond this local result slot.
  guard entry.scope > 0 && entry.link < 0 && entry.flags == 0 else { return -1 }
  symbol
}

///|
fn Program::single_statement(self : Program, id : Int) -> Int {
  for current = id {
    let node = self.nodes[current]
    guard node.kind is Block && node.b == 1 else { break current }
    continue self.edges[node.a]
  }
}

///|
fn Program::slot_assignment(
  self : Program,
  statement : Int,
  symbol : Int,
) -> Int {
  let node = self.nodes[self.single_statement(statement)]
  if node.kind is Block && node.b > 0 {
    for index in 0..<(node.b - 1) {
      guard self.nodes[self.edges[node.a + index]].kind is ExpressionStatement else {
        return -1
      }
    }
    return self.slot_assignment(self.edges[node.a + node.b - 1], symbol)
  }
  guard node.kind is ExpressionStatement else { return -1 }
  let expression = self.nodes[self.sequence_tail(node.a)]
  guard expression.kind is Assignment &&
    self.operators[expression.a] is Operator::Assignment &&
    self.identifier_symbol(expression.b) == symbol else {
    return -1
  }
  let value = self.nodes[self.unwrapped_expression(expression.c)]
  // Function names may change during compression. Keep inferred class names.
  match value.kind {
    ClassExpression if self.classes[value.a].name < 0 => -1
    _ => expression.c
  }
}

///|
fn Program::slot_assignment_value(
  self : Program,
  statement : Int,
  symbol : Int,
) -> Int {
  let node = self.nodes[self.single_statement(statement)]
  guard node.kind is Block else {
    let value = self.slot_assignment(statement, symbol)
    let tail = self.sequence_tail(node.a)
    guard tail != self.unwrapped_expression(node.a) else { return value }
    self.nodes[tail] = self.nodes[value]
    return node.a
  }
  let value = self.slot_assignment_value(
    self.edges[node.a + node.b - 1],
    symbol,
  )
  for index = node.b - 2, result = value {
    guard index >= 0 else { break result }
    let expression = self.nodes[self.edges[node.a + index]].a
    continue index - 1, self.sequence_pair(expression, result)
  }
}

///|
fn Program::sequence_tail(self : Program, id : Int) -> Int {
  for expression = self.unwrapped_expression(id) {
    let node = self.nodes[expression]
    guard node.kind is Sequence && node.b > 0 else { break expression }
    continue self.unwrapped_expression(self.edges[node.a + node.b - 1])
  }
}

///|
fn Program::slot_branch_writes(
  self : Program,
  statement : Int,
  symbol : Int,
) -> Int {
  let id = self.single_statement(statement)
  let node = self.nodes[id]
  guard node.kind is If && node.c >= 0 else {
    guard self.slot_assignment(id, symbol) >= 0 else { return -1 }
    return 1
  }
  let consequent = self.slot_branch_writes(node.b, symbol)
  guard consequent >= 0 else { return -1 }
  let alternative = self.slot_branch_writes(node.c, symbol)
  guard alternative >= 0 else { return -1 }
  consequent + alternative
}

///|
fn Program::compact_result_slots(
  self : Program,
  offset : Int,
  count : Int,
  bindings : ValueBindings,
) -> Int {
  for read = 0, kept = 0 {
    guard read < count else { break kept }
    let declaration = self.edges[offset + read]
    self.edges[offset + kept] = declaration
    let slot = self.result_slot(declaration)
    guard slot >= 0 && read + 2 < count else { continue read + 1, kept + 1 }
    let producer = self.edges[offset + read + 1]
    let consumer = self.edges[offset + read + 2]
    guard self.fold_result_slot(producer, consumer, slot, bindings) else {
      continue read + 1, kept + 1
    }
    let entry = self.symbols[slot]
    self.symbols[slot] = { ..entry, flags: entry.flags | SYMBOL_ELIDED, }
    self.edges[offset + kept] = {
      guard self.nodes[producer].kind is Switch else { consumer }
      producer
    }
    continue read + 3, kept + 1
  }
}

///|
fn Program::fold_result_slot(
  self : Program,
  producer : Int,
  consumer : Int,
  slot : Int,
  bindings : ValueBindings,
) -> Bool {
  let node = self.nodes[producer]
  guard node.kind is If else {
    guard node.kind is Switch &&
      self.nodes[consumer].kind is Return &&
      self.identifier_symbol(self.nodes[consumer].a) == slot else {
      return false
    }
    return self.fold_switch_slot(producer, slot)
  }
  let expression = self.first_statement_expression(consumer)
  let movement = ValueMovement::new(bindings, slot, producer, -1)
  let reference = self.leading_value_read(expression, movement)
  guard reference >= 0 else { return false }
  let writes = self.slot_branch_writes(producer, slot)
  // Account for declaration, all branch writes and this single consumer.
  // Any self-read, closure capture, extra use or extra declaration rejects it.
  guard writes > 0 && self.symbols[slot].use_count == writes + 2 else {
    return false
  }
  let value = self.slot_branch_value(producer, slot)
  self.nodes[reference] = if reference == expression &&
    self.nodes[consumer].kind is (Return | Throw | If | Switch) {
    self.nodes[value]
  } else {
    Node::new(Parenthesized, value, 0, 0, 0)
  }
  true
}

///|
fn Program::slot_branch_value(
  self : Program,
  statement : Int,
  slot : Int,
) -> Int {
  let id = self.single_statement(statement)
  let node = self.nodes[id]
  guard node.kind is If else { return self.slot_assignment_value(id, slot) }
  let consequent = self.slot_branch_value(node.b, slot)
  let alternative = self.slot_branch_value(node.c, slot)
  self.compact_choice(node.a, consequent, alternative)
}

///|
fn Program::is_boolean_value(self : Program, id : Int) -> Bool {
  let node = self.nodes[self.unwrapped_expression(id)]
  match node.kind {
    BooleanLiteral => true
    Unary => self.operators[node.a] is Operator::LogicalNot
    Binary =>
      match self.operators[node.a] {
        Equal
        | NotEqual
        | StrictlyEqual
        | StrictlyNotEqual
        | LessThan
        | LessThanOrEqual
        | GreaterThan
        | GreaterThanOrEqual
        | In
        | InstanceOf => true
        _ => false
      }
    Logical => self.is_boolean_value(node.b) && self.is_boolean_value(node.c)
    Conditional =>
      self.is_boolean_value(node.b) && self.is_boolean_value(node.c)
    _ => false
  }
}

///|
fn Program::boolean_literal(self : Program, id : Int, value : Bool) -> Bool {
  let node = self.nodes[self.unwrapped_expression(id)]
  node.kind is BooleanLiteral && (node.a == 1) == value
}

///|
fn Program::compact_choice(
  self : Program,
  condition : Int,
  consequent : Int,
  alternative : Int,
) -> Int {
  let consequent_is_true = self.boolean_literal(consequent, true)
  let alternative_is_false = self.boolean_literal(alternative, false)
  let consequent_is_false = self.boolean_literal(consequent, false)
  let alternative_is_true = self.boolean_literal(alternative, true)
  if consequent_is_false && alternative_is_true {
    return self.negated_value(condition)
  }
  if consequent_is_true && alternative_is_false {
    guard !self.is_boolean_value(condition) else { return condition }
    return self.negated_value(self.negated_value(condition))
  }
  if self.is_boolean_value(condition) {
    if alternative_is_false {
      guard !consequent_is_true else { return condition }
      return self.slot_logical(Operator::LogicalAnd, condition, consequent)
    } else if consequent_is_true {
      return self.slot_logical(Operator::LogicalOr, condition, alternative)
    } else if consequent_is_false {
      return self.slot_logical(
        Operator::LogicalAnd,
        self.negated_value(condition),
        alternative,
      )
    } else if alternative_is_true {
      return self.slot_logical(
        Operator::LogicalOr,
        self.negated_value(condition),
        consequent,
      )
    }
  }
  let condition_operand = self.slot_operand(condition, ConditionalTest)
  let yes = self.slot_operand(consequent, ConditionalArm)
  let no = self.slot_operand(alternative, ConditionalArm)
  self.node(Conditional, condition_operand, yes, no, 0)
}

///|
priv enum SlotOperand {
  ConditionalTest
  ConditionalArm
  LogicalOperand(Operator)
}

///|
fn Program::slot_operand(
  self : Program,
  id : Int,
  context : SlotOperand,
) -> Int {
  guard self.slot_needs_parentheses(id, context) else { return id }
  self.node(Parenthesized, id, 0, 0, 0)
}

///|
fn Program::slot_needs_parentheses(
  self : Program,
  id : Int,
  context : SlotOperand,
) -> Bool {
  let node = self.nodes[id]
  match context {
    ConditionalArm => node.kind is Sequence
    ConditionalTest | LogicalOperand(_) =>
      match node.kind {
        Assignment | Conditional | Sequence | ArrowExpression => true
        Unary => self.operators[node.a] is (Yield | YieldStar)
        Logical => {
          guard context is LogicalOperand(parent) else { false }
          self.operators[node.a] is NullishCoalescing ||
          (parent is LogicalAnd && self.operators[node.a] is LogicalOr)
        }
        _ => false
      }
  }
}

///|
fn Program::slot_logical(
  self : Program,
  operator : Operator,
  left : Int,
  right : Int,
) -> Int {
  let lhs = self.slot_operand(left, LogicalOperand(operator))
  let rhs = self.slot_operand(right, LogicalOperand(operator))
  self.node(Logical, self.add_operator(operator), lhs, rhs, 0)
}

///|
fn Program::slot_case_statements(self : Program, case_id : Int) -> EdgeRange {
  let clause = self.nodes[case_id]
  for offset = clause.b, count = clause.c {
    guard count == 1 else { break { offset, count, } }
    let statement = self.nodes[self.edges[offset]]
    guard statement.kind is Block else { break { offset, count, } }
    continue statement.a, statement.b
  }
}

///|
fn Program::fold_switch_slot(self : Program, id : Int, slot : Int) -> Bool {
  let node = self.nodes[id]
  guard node.c > 0 && self.symbols[slot].use_count == node.c + 2 else {
    return false
  }
  let mut has_default = false
  for index in 0..<node.c {
    let case_id = self.edges[node.b + index]
    let clause = self.nodes[case_id]
    if clause.a < 0 {
      has_default = true
    }
    let statements = self.slot_case_statements(case_id)
    guard statements.count == 2 &&
      self.slot_assignment(self.edges[statements.offset], slot) >= 0 else {
      return false
    }
    let exit = self.nodes[self.edges[statements.offset + 1]]
    guard exit.kind is Break && exit.a < 0 else { return false }
  }
  guard has_default else { return false }
  for index in 0..<node.c {
    let case_id = self.edges[node.b + index]
    let statements = self.slot_case_statements(case_id)
    let assignment = self.edges[statements.offset]
    let value = self.slot_assignment_value(assignment, slot)
    self.nodes[assignment] = Node::new(Return, value, 0, 0, 0)
    self.nodes[case_id] = { ..self.nodes[case_id], b: statements.offset, c: 1, }
  }
  true
}

///|
fn Program::remove_tail_continue(self : Program, id : Int) -> Unit {
  let node = self.nodes[id]
  match node.kind {
    Block if node.b > 0 => {
      let last = self.edges[node.a + node.b - 1]
      let tail = self.nodes[last]
      guard tail.kind is Continue && tail.a < 0 else {
        return self.remove_tail_continue(last)
      }
      self.nodes[id] = { ..node, b: node.b - 1, }
    }
    If => {
      self.remove_tail_continue(node.b)
      if node.c >= 0 {
        self.remove_tail_continue(node.c)
      }
    }
    Continue if node.a < 0 => self.nodes[id] = Node::new(Empty, 0, 0, 0, 0)
    // Nested loops, switches, labels and try/finally need their own control-flow
    // reasoning; a terminal continue inside them is intentionally left alone.
    _ => ()
  }
}
