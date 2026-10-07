///|
fn Program::unwrapped_expression(self : Program, id : Int) -> Int {
  for expression = id {
    guard expression >= 0 else { break expression }
    let node = self.nodes[expression]
    guard node.kind == Parenthesized else { break expression }
    continue node.a
  }
}

///|
fn Program::is_undefined_value(self : Program, id : Int) -> Bool {
  guard !self.has_dynamic_scope else { return false }
  let expression = self.unwrapped_expression(id)
  guard expression >= 0 else { return false }
  let node = self.nodes[expression]
  guard node.kind == Identifier else { return false }
  let reference = self.references[node.a]
  reference.symbol < 0 && self.names[reference.name] == "undefined"
}

///|
fn Program::is_number_literal(self : Program, id : Int) -> Bool {
  let expression = self.unwrapped_expression(id)
  let node = self.nodes[expression]
  node.kind == NumberLiteral && self.source[node.b - 1] != 'n'
}

///|
fn Program::is_zero_literal(self : Program, id : Int) -> Bool {
  let expression = self.unwrapped_expression(id)
  let node = self.nodes[expression]
  node.kind == NumberLiteral &&
  node.b - node.a == 1 &&
  self.source[node.a] == '0'
}

///|
/// MoonBit emits decimal shift counts. Counts 1 through 31 make an unsigned
/// right shift fit in Int32; count zero must keep the outer signed conversion.
fn Program::is_positive_shift_count(self : Program, id : Int) -> Bool {
  let expression = self.unwrapped_expression(id)
  let node = self.nodes[expression]
  guard node.kind == NumberLiteral else { return false }
  let length = node.b - node.a
  let first_digit = self.source[node.a]
  match length {
    1 => first_digit >= '1' && first_digit <= '9'
    2 => {
      let second_digit = self.source[node.a + 1]
      (
        (first_digit == '1' || first_digit == '2') &&
        second_digit >= '0' &&
        second_digit <= '9'
      ) ||
      (first_digit == '3' && (second_digit == '0' || second_digit == '1'))
    }
    _ => false
  }
}

///|
fn Program::has_signed_number_result(self : Program, id : Int) -> Bool {
  let expression = self.unwrapped_expression(id)
  let node = self.nodes[expression]
  guard node.kind == Binary else { return false }
  match self.operators[node.a] {
    UnsignedShiftRight => self.is_positive_shift_count(node.c)
    ShiftLeft | ShiftRight | BitwiseAnd | BitwiseOr | BitwiseXor =>
      // A Number operand excludes a successful BigInt operation. Keeping the
      // inner operation also preserves coercions and mixed-type exceptions.
      self.is_number_literal(node.b) || self.is_number_literal(node.c)
    _ => false
  }
}

///|
/// Run after reference resolution, as part of the shared postorder arena scan.
fn Program::compact_expression(self : Program, id : Int) -> Unit {
  let node = self.nodes[id]
  match node.kind {
    Return =>
      if self.is_undefined_value(node.a) {
        self.nodes[id] = { ..node, a: -1, }
      }
    Binary =>
      if self.operators[node.a] == BitwiseOr &&
        self.is_zero_literal(node.c) &&
        self.has_signed_number_result(node.b) {
        // Preserve explicit parentheses: the printer keeps the parser's
        // grouping instead of reconstructing precedence from changed nodes.
        self.nodes[id] = self.nodes[node.b]
      }
    While => {
      let condition = self.nodes[self.unwrapped_expression(node.a)]
      if condition.kind == BooleanLiteral && condition.a == 1 {
        self.nodes[id] = Node::new(For, -1, -1, -1, node.b)
      }
    }
    _ => ()
  }
}

///|
fn Program::remove_final_unit_return(self : Program, function_id : Int) -> Unit {
  let function = self.functions[function_id]
  guard (function.flags & FUNCTION_EXPRESSION_BODY) == 0 else { return }
  let body = self.nodes[function.body]
  guard body.kind == Block && body.b > 0 else { return }
  let last = self.nodes[self.edges[body.a + body.b - 1]]
  guard last.kind == Return && last.a < 0 else { return }
  self.nodes[function.body] = { ..body, b: body.b - 1, }
}
