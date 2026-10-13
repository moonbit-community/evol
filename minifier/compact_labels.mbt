///|
#valtype
priv struct LabelExitGuard {
  statement_index : Int32
  condition : Int32
  invert_condition : Bool
  failure_branch : Int32
  success_prefix : Int32
}

///|
/// L: { prefix; if (bad) { failure; break L } success }
/// becomes { prefix; if (!bad) { success } { failure } } when success always
/// exits beyond L. Keep both existing lexical environments; the new success
/// block contains no direct lexical declarations and needs no new scope.
fn Program::compact_label_exit(self : Program, id : Int32) -> Unit {
  guard !self.has_dynamic_scope else { return }
  let labeled = self.nodes[id]
  guard labeled.kind is Labeled else { return }
  let initial_body = self.nodes[labeled.c]
  guard initial_body.kind is Block else { return }
  let in_function = for scope = initial_body.c {
    guard scope >= 0 else { break false }
    let environment = self.scopes[scope]
    if environment.kind == SCOPE_FUNCTION {
      break true
    }
    continue environment.parent
  }
  // Script/eval observes statement completion values. These rules are for
  // function bodies, where ordinary statement values are discarded.
  guard in_function else { return }
  if self.trim_label_tail(labeled.c, labeled.d) &&
    !self.references_label(labeled.c, labeled.d) {
    self.nodes[id] = self.nodes[labeled.c]
    return
  }
  let body = self.nodes[labeled.c]
  guard body.kind is Block && body.b >= 2 else { return }
  if !self.is_forwarding_label_test(body, labeled.d) &&
    self.compact_simple_label_guards(id, labeled, body) {
    return
  }
  if self.compact_label_test(id, labeled, body) {
    return
  }
  let tail = self.edges[body.a + body.b - 1]
  guard self.exits_beyond_label(tail, labeled.d) else { return }
  guard self.label_exit_guard(body, labeled.d) is Some(exit_guard) else {
    return
  }
  for index in (exit_guard.statement_index + 1)..<body.b {
    guard self.can_nest_label_tail(self.edges[body.a + index]) else { return }
  }
  // Do not remove a label still targeted elsewhere, or move control flow
  // through finally, dynamic scope or Annex B function declarations.
  guard self.label_break_count(labeled.c, labeled.d) == 1 else { return }
  let tail_offset = body.a + exit_guard.statement_index + 1
  let tail_count = body.b - exit_guard.statement_index - 1
  let success_offset = if exit_guard.success_prefix >= 0 {
    let offset = self.edges.length()
    // Keep this branch as a separate statement: its lexical environment must
    // end before the original success tail starts executing.
    self.edges.push(exit_guard.success_prefix)
    for index in 0..<tail_count {
      self.edges.push(self.edges[tail_offset + index])
    }
    offset
  } else {
    tail_offset
  }
  let success_count = tail_count +
    (if exit_guard.success_prefix >= 0 { 1 } else { 0 })
  let success = self.node(Block, success_offset, success_count, body.c, body.d)
  let condition = if exit_guard.invert_condition {
    self.negated_value(exit_guard.condition)
  } else {
    exit_guard.condition
  }
  let success_guard = self.node(If, condition, success, -1, 0)
  let failure = self.nodes[exit_guard.failure_branch]
  let failure_body = if failure.kind is Block && failure.b > 1 {
    self.node(Block, failure.a, failure.b - 1, failure.c, failure.d)
  } else {
    -1
  }
  let offset = self.edges.length()
  for index in 0..<exit_guard.statement_index {
    self.edges.push(self.edges[body.a + index])
  }
  self.edges.push(success_guard)
  if failure_body >= 0 {
    self.edges.push(failure_body)
  }
  self.nodes[id] = {
    ..body,
    a: offset,
    b: exit_guard.statement_index + 1 + (if failure_body >= 0 { 1 } else { 0 }),
  }
}

///|
/// A guard containing only break L skips the rest of this exact label block.
/// Put that suffix under the inverted guard without changing its evaluation
/// order. Other exits keep the label, including exits inside loops or finally.
fn Program::compact_simple_label_guards(
  self : Program,
  id : Int32,
  labeled : Node,
  original_body : Node,
) -> Bool {
  let mut body = original_body
  let mut changed = false
  for index = body.b - 2; index >= 0; index = index - 1 {
    let branch = self.nodes[self.edges[body.a + index]]
    guard branch.kind is If && branch.c < 0 else { continue }
    let consequent = self.nodes[self.single_statement(branch.b)]
    guard consequent.kind is Break && consequent.c == labeled.d else {
      continue
    }
    guard self.can_nest_label_suffix(body, index + 1) else { continue }
    // Only a new empty lexical environment is introduced. Existing suffix
    // blocks retain their own scopes, and direct lexical declarations reject.
    let suffix = self.node(
      Block,
      body.a + index + 1,
      body.b - index - 1,
      body.c,
      body.d,
    )
    let condition = self.negated_value(branch.a)
    let guarded_suffix = self.node(If, condition, suffix, -1, 0)
    self.compact_branch(guarded_suffix)
    let offset = self.edges.length()
    for prefix in 0..<index {
      self.edges.push(self.edges[body.a + prefix])
    }
    self.edges.push(guarded_suffix)
    body = { ..body, a: offset, b: index + 1, }
    changed = true
  }
  guard changed else { return false }
  let body_id = self.node(Block, body.a, body.b, body.c, body.d)
  self.nodes[id] = if self.references_label(body_id, labeled.d) {
    { ..labeled, c: body_id, }
  } else {
    body
  }
  true
}

///|
fn Program::can_nest_label_suffix(
  self : Program,
  body : Node,
  start : Int32,
) -> Bool {
  for index in start..<body.b {
    guard self.can_nest_label_tail(self.edges[body.a + index]) else {
      return false
    }
  }
  true
}

///|
/// Leave a pure inner test chain intact for compact_label_test on its enclosing
/// label. Converting it to inverted guards first obscures the shorter OR form.
fn Program::is_forwarding_label_test(
  self : Program,
  body : Node,
  label : Int32,
) -> Bool {
  let tail = self.nodes[self.edges[body.a + body.b - 1]]
  guard tail.kind is Break && tail.c >= 0 && tail.c != label else {
    return false
  }
  for index in 0..<(body.b - 1) {
    guard self.is_label_test(self.edges[body.a + index], label) else {
      return false
    }
  }
  true
}

///|
/// Only these statement tails fall directly out of the enclosing label.
/// Loops, switches and exception handlers keep their own completion behavior.
fn Program::trim_label_tail(self : Program, id : Int32, label : Int32) -> Bool {
  let node = self.nodes[id]
  match node.kind {
    Break if node.c == label => {
      self.nodes[id] = Node::new(Empty, 0, 0, 0, 0)
      true
    }
    Block => {
      let mut kept = node.b
      let mut removed_break = false
      while kept > 0 {
        let tail = self.edges[node.a + kept - 1]
        if self.trim_label_tail(tail, label) {
          removed_break = true
        }
        guard self.is_empty_statement(self.single_statement(tail)) else {
          break
        }
        kept -= 1
      }
      if removed_break && kept != node.b {
        self.nodes[id] = { ..node, b: kept, }
      }
      removed_break
    }
    If => {
      let consequent_changed = self.trim_label_tail(node.b, label)
      let alternative_changed = node.c >= 0 &&
        self.trim_label_tail(node.c, label)
      guard consequent_changed || alternative_changed else { return false }
      let consequent_empty = self.is_empty_statement(
        self.single_statement(node.b),
      )
      let alternative_empty = node.c < 0 ||
        self.is_empty_statement(self.single_statement(node.c))
      if alternative_empty {
        self.nodes[id] = { ..node, c: -1, }
      } else if consequent_empty &&
        !(self.nodes[node.c].kind is FunctionDeclaration) {
        let condition = self.negated_value(node.a)
        self.nodes[id] = Node::new(If, condition, node.c, -1, 0)
      }
      self.compact_branch(id)
      true
    }
    Labeled if node.d != label => self.trim_label_tail(node.c, label)
    _ => false
  }
}

///|
/// Called only after a tail break was removed. Unlike tail trimming, this
/// reference check visits loop/switch/try bodies, where earlier exits remain
/// necessary. Function and class bodies have independent label namespaces.
fn Program::references_label(self : Program, id : Int32, label : Int32) -> Bool {
  guard id >= 0 else { return false }
  let node = self.nodes[id]
  match node.kind {
    Break | Continue => node.c == label
    Block => self.statements_reference_label(node.a, node.b, label)
    If =>
      self.references_label(node.b, label) ||
      self.references_label(node.c, label)
    Labeled => node.d == label || self.references_label(node.c, label)
    While | With => self.references_label(node.b, label)
    DoWhile => self.references_label(node.a, label)
    For => self.references_label(node.d, label)
    ForIn | ForOf => self.references_label(node.c, label)
    Switch => {
      for index in 0..<node.c {
        let clause = self.nodes[self.edges[node.b + index]]
        if self.statements_reference_label(clause.b, clause.c, label) {
          return true
        }
      }
      false
    }
    Try =>
      self.references_label(node.a, label) ||
      self.references_label(node.b, label) ||
      self.references_label(node.c, label)
    Catch => self.references_label(node.b, label)
    _ => false
  }
}

///|
fn Program::statements_reference_label(
  self : Program,
  offset : Int32,
  count : Int32,
  label : Int32,
) -> Bool {
  for index in 0..<count {
    if self.references_label(self.edges[offset + index], label) {
      return true
    }
  }
  false
}

///|
/// Outer: { Inner: { if (a) break Inner; if (b) break Inner; break Outer }
///          return value }
/// becomes if (a || b) return value. Recognition permits only tests and the
/// two explicit targets, so there are no declarations or finally boundaries.
fn Program::compact_label_test(
  self : Program,
  id : Int32,
  outer : Node,
  body : Node,
) -> Bool {
  guard body.b == 2 else { return false }
  let inner = self.nodes[self.edges[body.a]]
  let effect = self.edges[body.a + 1]
  guard inner.kind is Labeled &&
    inner.d != outer.d &&
    self.nodes[self.single_statement(effect)].kind is (Return | Throw) else {
    return false
  }
  let tests = self.nodes[inner.c]
  guard tests.kind is Block && tests.b >= 2 else { return false }
  let tail = self.nodes[self.edges[tests.a + tests.b - 1]]
  guard tail.kind is Break && tail.c == outer.d else { return false }
  for index in 0..<(tests.b - 1) {
    guard self.is_label_test(self.edges[tests.a + index], inner.d) else {
      return false
    }
  }
  let mut condition = self.label_test_condition(self.edges[tests.a], inner.d)
  for index in 1..<(tests.b - 1) {
    let next = self.label_test_condition(self.edges[tests.a + index], inner.d)
    condition = self.slot_logical(LogicalOr, condition, next)
  }
  self.nodes[id] = Node::new(If, condition, effect, -1, 0)
  true
}

///|
/// Every successful arm breaks to the same inner label. A false arm may only
/// fall through or test another condition; no other statement can be moved.
fn Program::is_label_test(self : Program, id : Int32, label : Int32) -> Bool {
  let branch = self.nodes[self.single_statement(id)]
  guard branch.kind is If else { return false }
  let consequent = self.nodes[self.single_statement(branch.b)]
  let then_breaks = consequent.kind is Break && consequent.c == label
  guard then_breaks || self.is_label_test(branch.b, label) else { return false }
  branch.c < 0 || self.is_label_test(branch.c, label)
}

///|
fn Program::label_test_condition(
  self : Program,
  id : Int32,
  label : Int32,
) -> Int32 {
  let branch = self.nodes[self.single_statement(id)]
  let consequent = self.nodes[self.single_statement(branch.b)]
  let then_breaks = consequent.kind is Break && consequent.c == label
  if then_breaks {
    guard branch.c >= 0 else { return branch.a }
    let alternative = self.label_test_condition(branch.c, label)
    return self.slot_logical(LogicalOr, branch.a, alternative)
  }
  let consequent_test = self.label_test_condition(branch.b, label)
  guard branch.c >= 0 else {
    return self.slot_logical(LogicalAnd, branch.a, consequent_test)
  }
  let alternative_test = self.label_test_condition(branch.c, label)
  let condition = self.slot_operand(branch.a, ConditionalTest)
  let consequent_value = self.slot_operand(consequent_test, ConditionalArm)
  let alternative_value = self.slot_operand(alternative_test, ConditionalArm)
  self.node(Conditional, condition, consequent_value, alternative_value, 0)
}

///|
fn Program::label_exit_guard(
  self : Program,
  body : Node,
  label : Int32,
) -> LabelExitGuard? {
  for index in 0..<(body.b - 1) {
    let branch = self.nodes[self.edges[body.a + index]]
    guard branch.kind is If else { continue }
    if self.has_direct_label_break_tail(branch.b, label) {
      return Some({
        statement_index: index,
        condition: branch.a,
        invert_condition: true,
        failure_branch: branch.b,
        success_prefix: branch.c,
      })
    }
    if branch.c >= 0 && self.has_direct_label_break_tail(branch.c, label) {
      return Some({
        statement_index: index,
        condition: branch.a,
        invert_condition: false,
        failure_branch: branch.c,
        success_prefix: branch.b,
      })
    }
  }
  None
}

///|
fn Program::has_direct_label_break_tail(
  self : Program,
  id : Int32,
  label : Int32,
) -> Bool {
  let branch = self.nodes[id]
  let tail = if branch.kind is Block && branch.b > 0 {
    self.nodes[self.edges[branch.a + branch.b - 1]]
  } else {
    branch
  }
  tail.kind is Break && tail.c == label
}

///|
/// The terminal jump must leave the candidate label. A nested loop, switch,
/// label or try statement needs its own target/finally analysis and is skipped.
fn Program::exits_beyond_label(
  self : Program,
  id : Int32,
  label : Int32,
) -> Bool {
  let node = self.nodes[id]
  match node.kind {
    Return | Throw => true
    Break | Continue => node.c != label
    Block if node.b > 0 =>
      self.exits_beyond_label(self.edges[node.a + node.b - 1], label)
    If =>
      node.c >= 0 &&
      self.exits_beyond_label(node.b, label) &&
      self.exits_beyond_label(node.c, label)
    _ => false
  }
}

///|
/// Moving a lexical declaration into a new conditional block changes its TDZ
/// and visibility to the prefix. Nested blocks retain their existing scopes.
fn Program::can_nest_label_tail(self : Program, id : Int32) -> Bool {
  let node = self.nodes[id]
  match node.kind {
    VariableDeclaration => node.a == DECLARATION_VAR
    FunctionDeclaration | ClassDeclaration => false
    Labeled => self.can_nest_label_tail(node.c)
    _ => true
  }
}

///|
/// Count only statement-level jumps; function/class expressions introduce
/// independent label namespaces. Negative means an unsupported boundary.
fn Program::label_break_count(
  self : Program,
  id : Int32,
  label : Int32,
) -> Int32 {
  guard id >= 0 else { return 0 }
  let node = self.nodes[id]
  match node.kind {
    Break => if node.c == label { 1 } else { 0 }
    Continue => if node.c == label { -1 } else { 0 }
    Block => self.label_breaks_in_statements(node.a, node.b, label)
    If => {
      let consequent = self.label_break_count(node.b, label)
      guard consequent >= 0 && consequent <= 1 else { return consequent }
      let alternative = self.label_break_count(node.c, label)
      guard alternative >= 0 else { return alternative }
      consequent + alternative
    }
    Labeled => {
      guard node.d != label else { return -1 }
      self.label_break_count(node.c, label)
    }
    While => self.label_break_count(node.b, label)
    DoWhile => self.label_break_count(node.a, label)
    For => self.label_break_count(node.d, label)
    ForIn | ForOf => self.label_break_count(node.c, label)
    Switch => {
      let mut breaks = 0
      for index in 0..<node.c {
        let clause = self.nodes[self.edges[node.b + index]]
        let case_breaks = self.label_breaks_in_statements(
          clause.b,
          clause.c,
          label,
        )
        guard case_breaks >= 0 else { return case_breaks }
        breaks += case_breaks
        guard breaks <= 1 else { return breaks }
      }
      breaks
    }
    Try | Catch | With | FunctionDeclaration | ClassDeclaration => -1
    _ => 0
  }
}

///|
fn Program::label_breaks_in_statements(
  self : Program,
  offset : Int32,
  count : Int32,
  label : Int32,
) -> Int32 {
  let mut breaks = 0
  for index in 0..<count {
    let statement_breaks = self.label_break_count(
      self.edges[offset + index],
      label,
    )
    guard statement_breaks >= 0 else { return statement_breaks }
    breaks += statement_breaks
    guard breaks <= 1 else { return breaks }
  }
  breaks
}
