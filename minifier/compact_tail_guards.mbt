///|
/// if (test) { effects } else return x; return y
/// becomes return test ? (effects, y) : x. Only expression-only arms can be
/// moved: declarations and other control flow keep their original statements.
fn Program::compact_value_branch_return(
  self : Program,
  branch_id : Int32,
  return_id : Int32,
) -> Bool {
  let branch = self.nodes[branch_id]
  let following = self.nodes[return_id]
  let consequent = self.nodes[self.single_statement(branch.b)]
  let alternative = self.nodes[self.single_statement(branch.c)]
  let (yes, no) = if consequent.kind is Return &&
    consequent.a >= 0 &&
    self.is_expression_prefix(branch.c) {
    (consequent.a, self.prefix_return_value(branch.c, following.a))
  } else if alternative.kind is Return &&
    alternative.a >= 0 &&
    self.is_expression_prefix(branch.b) {
    (self.prefix_return_value(branch.b, following.a), alternative.a)
  } else {
    return false
  }
  let value = self.compact_choice(branch.a, yes, no)
  self.nodes[return_id] = { ..following, a: value, }
  true
}

///|
fn Program::is_expression_prefix(self : Program, id : Int32) -> Bool {
  let statement = self.nodes[self.single_statement(id)]
  match statement.kind {
    Empty | ExpressionStatement => true
    Block => {
      for index in 0..<statement.b {
        guard self.nodes[self.edges[statement.a + index]].kind
          is ExpressionStatement else {
          return false
        }
      }
      true
    }
    _ => false
  }
}

///|
/// Called only after the prefix is known to contain no bindings or exits.
fn Program::prefix_return_value(
  self : Program,
  id : Int32,
  result : Int32,
) -> Int32 {
  let statement = self.nodes[self.single_statement(id)]
  match statement.kind {
    ExpressionStatement => self.sequence_pair(statement.a, result)
    Block if statement.b > 0 => {
      let offset = self.edges.length()
      for index in 0..<statement.b {
        self.edges.push(self.nodes[self.edges[statement.a + index]].a)
      }
      self.edges.push(result)
      self.node(Sequence, offset, statement.b + 1, 0, 0)
    }
    _ => result
  }
}

///|
/// Share the bare return while retaining the effect arm's lexical block:
/// if (test) return; else { effects; return } becomes a guarded effect block
/// followed by return. The enclosing statement need not be a function tail.
fn Program::compact_unit_branch(self : Program, id : Int32) -> Unit {
  let branch = self.nodes[id]
  guard branch.kind is If && branch.c >= 0 else { return }
  let (effects_id, invert_condition) = if self.is_bare_return_statement(
      branch.b,
    ) &&
    self.nodes[branch.c].kind is Block {
    (branch.c, true)
  } else if self.is_bare_return_statement(branch.c) &&
    self.nodes[branch.b].kind is Block {
    (branch.b, false)
  } else {
    return
  }
  guard self.has_bare_return_tail(effects_id) else { return }
  let original_effects = self.nodes[effects_id]
  if original_effects.b == 2 {
    let first = self.nodes[self.edges[original_effects.a]]
    let tail = self.nodes[self.edges[original_effects.a + 1]]
    // A child If may already have become this synthetic shape. Its scope is
    // inherited, so it must not be treated as another owned block scope.
    // Conservatively leave matching source blocks alone as well.
    if first.kind is If && tail.kind is Return && tail.a < 0 {
      return
    }
  }
  let effects = self.without_bare_return_tail(effects_id)
  let condition = if invert_condition {
    self.negated_value(branch.a)
  } else {
    branch.a
  }
  let guarded_effects = self.node(If, condition, effects, -1, 0)
  let shared_return = self.node(Return, -1, 0, 0, 0)
  let offset = self.edges.length()
  self.edges.push(guarded_effects)
  self.edges.push(shared_return)
  let scope = self.scopes[original_effects.c].parent
  // This block adds no bindings; preserve the original effect block below it.
  // A positive literal summary keeps the moved condition visible to pooling.
  self.nodes[id] = Node::new(Block, offset, 2, scope, 1)
}

///|
fn Program::is_bare_return_statement(self : Program, id : Int32) -> Bool {
  let statement = self.nodes[self.single_statement(id)]
  statement.kind is Return && statement.a < 0
}

///|
/// if (test) return; { effects; return }
/// becomes if (!test) { effects } return. The shared return remains in place:
/// this statement list need not be at the end of its function.
fn Program::compact_guarded_unit_return(
  self : Program,
  guard_id : Int32,
  following_id : Int32,
) -> Bool {
  let branch = self.nodes[guard_id]
  guard branch.kind is If && branch.c < 0 else { return false }
  let early_return = self.nodes[self.single_statement(branch.b)]
  guard early_return.kind is Return && early_return.a < 0 else { return false }
  guard self.has_bare_return_tail(following_id) else { return false }
  let effects = self.without_bare_return_tail(following_id)
  let condition = self.negated_value(branch.a)
  self.nodes[guard_id] = Node::new(If, condition, effects, -1, 0)
  self.nodes[following_id] = Node::new(Return, -1, 0, 0, 0)
  true
}

///|
/// Follow existing lexical blocks only. A finally, loop, switch or conditional
/// can alter completion and is deliberately outside this rule.
fn Program::has_bare_return_tail(self : Program, id : Int32) -> Bool {
  for block_id = id {
    let block = self.nodes[block_id]
    guard block.kind is Block && block.b > 0 else { break false }
    let tail_id = self.edges[block.a + block.b - 1]
    let tail = self.nodes[tail_id]
    match tail.kind {
      Return => break tail.a < 0
      Block => continue tail_id
      _ => break false
    }
  }
}

///|
/// Called only after has_bare_return_tail accepted the chain. Copy each block
/// on that path so other AST owners retain their original statement lists.
fn Program::without_bare_return_tail(self : Program, id : Int32) -> Int32 {
  let block = self.nodes[id]
  let prefix_count = block.b - 1
  let tail_id = self.edges[block.a + prefix_count]
  let tail = self.nodes[tail_id]
  if tail.kind is Return {
    return self.node(Block, block.a, prefix_count, block.c, block.d)
  }
  let trimmed_tail = self.without_bare_return_tail(tail_id)
  if self.nodes[trimmed_tail].b == 0 {
    // This nested block contained only the removed return and empty blocks.
    return self.node(Block, block.a, prefix_count, block.c, block.d)
  }
  let offset = self.edges.length()
  for index in 0..<prefix_count {
    self.edges.push(self.edges[block.a + index])
  }
  self.edges.push(trimmed_tail)
  self.node(Block, offset, block.b, block.c, block.d)
}
