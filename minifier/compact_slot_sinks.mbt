///|
/// Move the sole consumer into a producer's final assignment, retaining every
/// existing lexical block. Prefix declarations and their values stay in place.
fn Program::sink_result_slot(
  self : Program,
  producer : Int32,
  consumer : Int32,
  offset : Int32,
  producer_index : Int32,
  bindings : ValueBindings,
) -> Bool {
  let following = self.nodes[consumer]
  guard following.kind is (Return | ExpressionStatement) && following.a >= 0 else {
    return false
  }
  let leaf = self.result_slot_tail(producer)
  guard leaf >= 0 else { return false }
  let statement = self.nodes[leaf]
  let assignment = self.nodes[self.sequence_tail(statement.a)]
  guard assignment.kind is Assignment &&
    self.operators[assignment.a] is Operator::Assignment else {
    return false
  }
  let target = self.nodes[self.unwrapped_expression(assignment.b)]
  guard target.kind is Identifier else { return false }
  let destination = self.references[target.a].scope
  let slot = self.references[target.a].symbol
  guard slot >= 0 else { return false }
  let declaration = bindings.declarations[slot]
  guard declaration >= 0 && self.result_slot(declaration) == slot else {
    return false
  }
  let entry = self.symbols[slot]
  guard entry.use_count == 3 &&
    self.scopes[entry.scope].kind != SCOPE_SWITCH &&
    !self.preserved_names.contains(entry.name) &&
    self.slot_assignment(leaf, slot) >= 0 else {
    return false
  }
  let movement = ValueMovement::new(bindings, slot, producer, -1)
  let read = self.leading_value_read(following.a, movement)
  guard read >= 0 && self.references[self.nodes[read].a].scope == entry.scope else {
    return false
  }
  let declared_here = for previous = producer_index - 1 {
    guard previous >= 0 else { break false }
    if self.edges[offset + previous] == declaration {
      break true
    }
    continue previous - 1
  }
  guard declared_here else { return false }
  let references : Array[Int32] = []
  guard self.collect_slot_sink_references(
    following.a,
    read,
    destination,
    references,
  ) else {
    return false
  }
  // Update only the consumer's references. The initializer is still in its
  // original scope and is inserted after this relocation has finished.
  for index in references {
    self.references[index] = { ..self.references[index], scope: destination, }
  }
  self.mark_result_slot_tail_literals(producer)
  let value = self.slot_assignment_value(leaf, slot)
  let initializer = self.nodes[self.unwrapped_expression(value)]
  self.nodes[read] = if movement.detach_receiver &&
    initializer.kind is (Identifier | Member | Index) {
    let zero = self.node(ZeroLiteral, 0, 0, 0, 0)
    let detached = self.sequence_pair(zero, value)
    Node::new(Parenthesized, detached, 0, 0, 0)
  } else if read == following.a &&
    following.kind is ExpressionStatement &&
    initializer.kind is StringLiteral {
    Node::new(Parenthesized, value, 0, 0, 0)
  } else if self.is_primary_value(value) ||
    (read == following.a && following.kind is Return) {
    self.nodes[value]
  } else {
    Node::new(Parenthesized, value, 0, 0, 0)
  }
  self.nodes[leaf] = following
  self.nodes[consumer] = Node::new(Elided, 0, 0, 0, 0)
  self.nodes[declaration] = Node::new(Elided, 0, 0, 0, 0)
  self.symbols[slot] = { ..entry, flags: entry.flags | SYMBOL_ELIDED, }
  true
}

///|
/// Only direct block tails and a conditional whose other arm exits can reach
/// this leaf. A loop, switch, label or try needs different completion analysis.
fn Program::result_slot_tail(self : Program, id : Int32) -> Int32 {
  let node = self.nodes[id]
  match node.kind {
    ExpressionStatement => id
    Block if node.b > 0 =>
      self.result_slot_tail(self.edges[node.a + node.b - 1])
    If if node.c >= 0 =>
      if self.result_slot_exits(node.b) {
        self.result_slot_tail(node.c)
      } else if self.result_slot_exits(node.c) {
        self.result_slot_tail(node.b)
      } else {
        -1
      }
    _ => -1
  }
}

///|
fn Program::result_slot_exits(self : Program, id : Int32) -> Bool {
  let node = self.nodes[id]
  match node.kind {
    Return | Throw => true
    Block if node.b > 0 =>
      self.result_slot_exits(self.edges[node.a + node.b - 1])
    If =>
      node.c >= 0 &&
      self.result_slot_exits(node.b) &&
      self.result_slot_exits(node.c)
    _ => false
  }
}

///|
/// A moved consumer may introduce literals into a previously literal-free
/// block. Keep the literal-pool traversal's early-out summary conservative.
fn Program::mark_result_slot_tail_literals(self : Program, id : Int32) -> Unit {
  let node = self.nodes[id]
  match node.kind {
    Block => {
      if node.d == 0 {
        self.nodes[id] = { ..node, d: 1, }
      }
      self.mark_result_slot_tail_literals(self.edges[node.a + node.b - 1])
    }
    If =>
      if self.result_slot_exits(node.b) {
        self.mark_result_slot_tail_literals(node.c)
      } else {
        self.mark_result_slot_tail_literals(node.b)
      }
    _ => ()
  }
}

///|
/// Canonical binding names matter here: an eliminated identifier alias can
/// leave a reference's source spelling different from its target symbol name.
fn Program::collect_slot_sink_references(
  self : Program,
  id : Int32,
  replaced_read : Int32,
  destination : Int32,
  references : Array[Int32],
) -> Bool {
  guard id >= 0 && id != replaced_read else { return true }
  let node = self.nodes[id]
  match node.kind {
    Identifier => {
      let reference = self.references[node.a]
      let target = if reference.symbol >= 0 {
        self.symbol_root(reference.symbol)
      } else {
        -1
      }
      let name = if target >= 0 {
        self.symbols[target].name
      } else {
        reference.name
      }
      let binding = self.find_binding(destination, name)
      let same_binding = if target >= 0 {
        binding >= 0 && self.symbol_root(binding) == target
      } else {
        binding < 0
      }
      guard same_binding else { return false }
      references.push(node.a)
      true
    }
    NumberLiteral
    | ZeroLiteral
    | StringLiteral
    | RegexLiteral
    | BooleanLiteral
    | NullLiteral
    | This
    | Super => true
    Parenthesized | EvaluatedValue | Member | Spread =>
      self.collect_slot_sink_references(
        node.a,
        replaced_read,
        destination,
        references,
      )
    Index =>
      self.collect_slot_sink_references(
        node.a,
        replaced_read,
        destination,
        references,
      ) &&
      self.collect_slot_sink_references(
        node.b,
        replaced_read,
        destination,
        references,
      )
    Unary =>
      self.collect_slot_sink_references(
        node.b,
        replaced_read,
        destination,
        references,
      )
    Update =>
      self.collect_slot_sink_references(
        node.c,
        replaced_read,
        destination,
        references,
      )
    Binary | Logical | Assignment =>
      self.collect_slot_sink_references(
        node.b,
        replaced_read,
        destination,
        references,
      ) &&
      self.collect_slot_sink_references(
        node.c,
        replaced_read,
        destination,
        references,
      )
    Conditional =>
      self.collect_slot_sink_references(
        node.a,
        replaced_read,
        destination,
        references,
      ) &&
      self.collect_slot_sink_references(
        node.b,
        replaced_read,
        destination,
        references,
      ) &&
      self.collect_slot_sink_references(
        node.c,
        replaced_read,
        destination,
        references,
      )
    Call | New =>
      self.collect_slot_sink_references(
        node.a,
        replaced_read,
        destination,
        references,
      ) &&
      (
        node.b < 0 ||
        self.collect_slot_sink_list(
          node.b,
          node.c,
          replaced_read,
          destination,
          references,
        )
      )
    ArrayLiteral | ObjectLiteral | Sequence =>
      self.collect_slot_sink_list(
        node.a,
        node.b,
        replaced_read,
        destination,
        references,
      )
    Property =>
      (
        node.d != PROPERTY_COMPUTED ||
        self.collect_slot_sink_references(
          node.a,
          replaced_read,
          destination,
          references,
        )
      ) &&
      self.collect_slot_sink_references(
        node.b,
        replaced_read,
        destination,
        references,
      )
    // Functions, methods, classes and templates can own additional scopes.
    // Binding patterns and other statement nodes are outside this expression.
    _ => false
  }
}

///|
fn Program::collect_slot_sink_list(
  self : Program,
  offset : Int32,
  count : Int32,
  replaced_read : Int32,
  destination : Int32,
  references : Array[Int32],
) -> Bool {
  for index in 0..<count {
    guard self.collect_slot_sink_references(
      self.edges[offset + index],
      replaced_read,
      destination,
      references,
    ) else {
      return false
    }
  }
  true
}
