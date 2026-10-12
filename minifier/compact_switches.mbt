///|
fn Program::same_value_expression(
  self : Program,
  left : Int,
  right : Int,
) -> Bool {
  guard left >= 0 && right >= 0 else { return left == right }
  let left_value = self.unwrapped_expression(left)
  let right_value = self.unwrapped_expression(right)
  // Parentheses can end an optional chain before a surrounding access/call.
  if (left != left_value) != (right != right_value) &&
    (
      self.has_optional_chain(left_value) ||
      self.has_optional_chain(right_value)
    ) {
    return false
  }
  let lhs = self.nodes[left_value]
  let rhs = self.nodes[right_value]
  guard lhs.kind == rhs.kind else { return false }
  match lhs.kind {
    Identifier => {
      let a = self.references[lhs.a]
      let b = self.references[rhs.a]
      a.symbol == b.symbol && (a.symbol >= 0 || a.name == b.name)
    }
    NumberLiteral | StringLiteral =>
      self.source[lhs.a:lhs.b] == self.source[rhs.a:rhs.b]
    BooleanLiteral => lhs.a == rhs.a
    NullLiteral | ZeroLiteral | This => true
    Member =>
      lhs.d == rhs.d &&
      self.source[lhs.b:lhs.c] == self.source[rhs.b:rhs.c] &&
      self.same_value_expression(lhs.a, rhs.a)
    Index =>
      lhs.d == rhs.d &&
      self.same_value_expression(lhs.a, rhs.a) &&
      self.same_value_expression(lhs.b, rhs.b)
    Unary =>
      self.operators[lhs.a] == self.operators[rhs.a] &&
      self.same_value_expression(lhs.b, rhs.b)
    Binary | Logical | Assignment =>
      self.operators[lhs.a] == self.operators[rhs.a] &&
      self.same_value_expression(lhs.b, rhs.b) &&
      self.same_value_expression(lhs.c, rhs.c)
    Call =>
      lhs.d == rhs.d &&
      self.same_value_expression(lhs.a, rhs.a) &&
      self.same_expression_list(lhs.b, lhs.c, rhs.b, rhs.c)
    Sequence => self.same_expression_list(lhs.a, lhs.b, rhs.a, rhs.b)
    EvaluatedValue => self.same_value_expression(lhs.a, rhs.a)
    _ => false
  }
}

///|
fn Program::same_expression_list(
  self : Program,
  left_offset : Int,
  left_count : Int,
  right_offset : Int,
  right_count : Int,
) -> Bool {
  guard left_count == right_count else { return false }
  for index in 0..<left_count {
    guard self.same_value_expression(
      self.edges[left_offset + index],
      self.edges[right_offset + index],
    ) else {
      return false
    }
  }
  true
}

///|
fn Program::case_return_value(self : Program, id : Int) -> Int {
  let clause = self.nodes[id]
  guard clause.c == 1 else { return -1 }
  let statement = self.nodes[self.single_statement(self.edges[clause.b])]
  guard statement.kind is Return else { return -1 }
  statement.a
}

///|
fn Program::compact_switch_returns(self : Program, id : Int) -> Unit {
  let statement = self.nodes[id]
  guard statement.c > 0 else { return }
  let last = self.edges[statement.b + statement.c - 1]
  let result = self.case_return_value(last)
  let mut all_same = result >= 0
  let mut has_default = false
  let mut literal_cases = true
  for index in 0..<statement.c {
    let case_id = self.edges[statement.b + index]
    let clause = self.nodes[case_id]
    if clause.a < 0 {
      has_default = true
    } else if !(self.nodes[clause.a].kind
      is (NumberLiteral | StringLiteral | BooleanLiteral | NullLiteral)) {
      literal_cases = false
    }
    if clause.c > 0 &&
      !self.same_value_expression(result, self.case_return_value(case_id)) {
      all_same = false
    }
  }
  if all_same && has_default && literal_cases {
    let value = self.sequence_pair(statement.a, result)
    self.nodes[id] = Node::new(Return, value, 0, 0, 0)
    return
  }
  for index = statement.c - 2, following = result {
    guard index >= 0 else { break }
    let case_id = self.edges[statement.b + index]
    let clause = self.nodes[case_id]
    guard clause.c > 0 else { continue index - 1, following }
    let value = self.case_return_value(case_id)
    if value >= 0 &&
      following >= 0 &&
      self.same_value_expression(value, following) {
      self.nodes[case_id] = { ..clause, c: 0, }
    }
    continue index - 1, value
  }
}
