///|
fn Program::unwrapped_expression(self : Program, id : Int32) -> Int32 {
  for expression = id {
    guard expression >= 0 else { break expression }
    let node = self.nodes[expression]
    guard node.kind is Parenthesized else { break expression }
    continue node.a
  }
}

///|
/// These atoms keep their meaning without grouping. Strings can be directives;
/// let/async need grouping in some for headers. Optional chains are not atoms.
fn Program::compact_parenthesized(self : Program, id : Int32) -> Unit {
  let parenthesized = self.nodes[id]
  guard parenthesized.kind is Parenthesized else { return }
  let value = self.nodes[parenthesized.a]
  match value.kind {
    Identifier => {
      let name = self.names[self.references[value.a].name]
      guard name != "let" && name != "async" else { return }
    }
    NumberLiteral | BooleanLiteral | NullLiteral | This => ()
    _ => return
  }
  self.nodes[id] = value
}

///|
fn Program::is_undefined_value(self : Program, id : Int32) -> Bool {
  guard !self.has_dynamic_scope else { return false }
  let expression = self.unwrapped_expression(id)
  guard expression >= 0 else { return false }
  let node = self.nodes[expression]
  guard node.kind is Identifier else { return false }
  let reference = self.references[node.a]
  reference.symbol < 0 && reference.name == NAME_UNDEFINED
}

///|
fn Program::is_number_literal(self : Program, id : Int32) -> Bool {
  let expression = self.unwrapped_expression(id)
  let node = self.nodes[expression]
  node.kind is NumberLiteral && self.source[node.b - 1] != 'n'
}

///|
fn Program::is_zero_literal(self : Program, id : Int32) -> Bool {
  let expression = self.unwrapped_expression(id)
  let node = self.nodes[expression]
  node.kind is NumberLiteral &&
  node.b - node.a == 1 &&
  self.source[node.a] == '0'
}

///|
/// MoonBit emits decimal shift counts. Counts 1 through 31 make an unsigned
/// right shift fit in Int32; count zero must keep the outer signed conversion.
fn Program::is_positive_shift_count(self : Program, id : Int32) -> Bool {
  let expression = self.unwrapped_expression(id)
  let node = self.nodes[expression]
  guard node.kind is NumberLiteral else { return false }
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
fn Program::has_signed_number_result(self : Program, id : Int32) -> Bool {
  let expression = self.unwrapped_expression(id)
  let node = self.nodes[expression]
  guard node.kind is Binary else { return false }
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
/// Compare a plain decimal spelling with a decimal bound without parsing or
/// arithmetic overflow. Multi-digit leading zeroes may be legacy octal.
fn Program::decimal_integer_literal_within(
  self : Program,
  literal : Node,
  maximum : String,
) -> Bool {
  let length = literal.b - literal.a
  guard length > 0 && length <= maximum.length() else { return false }
  guard length == 1 || self.source[literal.a] != '0' else { return false }
  for index in literal.a..<literal.b {
    guard self.source[index] is ('0'..<='9') else { return false }
  }
  guard length == maximum.length() else { return true }
  for index in 0..<length {
    let digit = self.source[literal.a + index]
    let limit_digit = maximum[index]
    if digit < limit_digit {
      return true
    }
    if digit > limit_digit {
      return false
    }
  }
  true
}

///|
/// Run after reference resolution, as part of the shared postorder arena scan.
fn Program::compact_expression(self : Program, id : Int32) -> Unit {
  let node = self.nodes[id]
  match node.kind {
    Return if self.is_undefined_value(node.a) =>
      self.nodes[id] = { ..node, a: -1, }
    Unary if self.operators[node.a] is LogicalNot => {
      let value = self.simplified_negation(node.b)
      guard value >= 0 else { return }
      let result = self.nodes[value]
      self.nodes[id] = if result.kind
        is (Binary | Logical | Conditional | Sequence) {
        Node::new(Parenthesized, value, 0, 0, 0)
      } else {
        result
      }
    }
    Conditional => {
      let alternative = self.nodes[self.unwrapped_expression(node.c)]
      if alternative.kind is Conditional &&
        self.same_value_expression(node.b, alternative.b) {
        let condition = self.slot_logical(LogicalOr, node.a, alternative.a)
        let value = self.compact_choice(condition, node.b, alternative.c)
        self.nodes[id] = self.nodes[value]
        return
      }
      let consequent = self.nodes[self.unwrapped_expression(node.b)]
      guard consequent.kind is BooleanLiteral ||
        alternative.kind is BooleanLiteral else {
        return
      }
      let value = self.compact_choice(node.a, node.b, node.c)
      self.nodes[id] = self.nodes[value]
    }
    Binary if self.nodes[node.b].kind is NumberLiteral => {
      let maximum = match self.operators[node.a] {
        UnsignedShiftRight => "4294967295"
        BitwiseOr | ShiftRight => "2147483647"
        _ => return
      }
      guard self.is_zero_literal(node.c) &&
        self.decimal_integer_literal_within(self.nodes[node.b], maximum) else {
        return
      }
      self.nodes[id] = self.nodes[node.b]
    }
    Binary if self.operators[node.a]
      is (BitwiseOr | ShiftRight | UnsignedShiftRight) &&
      self.is_zero_literal(node.c) => {
      let inner = self.nodes[self.unwrapped_expression(node.b)]
      if inner.kind is Binary &&
        self.operators[inner.a] is (BitwiseOr | ShiftRight | UnsignedShiftRight) &&
        self.is_zero_literal(inner.c) {
        let operand = self.integer_coercion_operand(
          inner.b,
          self.operators[node.a],
        )
        self.nodes[id] = { ..node, b: operand, }
      } else if self.operators[node.a] is BitwiseOr &&
        self.has_signed_number_result(node.b) {
        // Preserve explicit parentheses when dropping the outer operator.
        self.nodes[id] = self.nodes[node.b]
      }
    }
    While => {
      let condition = self.nodes[self.unwrapped_expression(node.a)]
      guard condition.kind is BooleanLiteral && condition.a == 1 else { return }
      self.nodes[id] = Node::new(For, -1, -1, -1, node.b)
    }
    _ => ()
  }
}

///|
/// Keep the inner operand's grouping. A shift binds more tightly than a
/// bitwise operand, so (x | y | 0) >>> 0 must become (x | y) >>> 0.
fn Program::integer_coercion_operand(
  self : Program,
  id : Int32,
  operator : Operator,
) -> Int32 {
  let value = self.nodes[id]
  let needs_parentheses = match value.kind {
    Assignment | Conditional | Logical | Sequence | ArrowExpression => true
    Unary => self.operators[value.a] is (Yield | YieldStar)
    Binary if !(operator is BitwiseOr) =>
      !(self.operators[value.a]
      is (Add
      | Subtract
      | Multiply
      | Divide
      | Modulo
      | Exponent
      | ShiftLeft
      | ShiftRight
      | UnsignedShiftRight))
    _ => false
  }
  if needs_parentheses {
    self.node(Parenthesized, id, 0, 0, 0)
  } else {
    id
  }
}

///|
fn Program::negated_value(self : Program, id : Int32) -> Int32 {
  let simplified = self.simplified_negation(id)
  guard simplified < 0 else { return simplified }
  let node = self.nodes[self.unwrapped_expression(id)]
  let operand = if self.is_primary_value(id) ||
    node.kind is (NumberLiteral | NullLiteral) ||
    (node.kind is Unary && !(self.operators[node.a] is (Yield | YieldStar))) {
    id
  } else {
    self.node(Parenthesized, id, 0, 0, 0)
  }
  self.node(Unary, self.add_operator(LogicalNot), operand, 0, 0)
}

///|
fn Program::simplified_negation(self : Program, id : Int32) -> Int32 {
  let expression = self.unwrapped_expression(id)
  let node = self.nodes[expression]
  match node.kind {
    BooleanLiteral => return self.node(BooleanLiteral, 1 - node.a, 0, 0, 0)
    Unary if self.operators[node.a] is LogicalNot &&
      self.is_boolean_value(node.b) => return node.b
    Binary => {
      let opposite : Operator? = match self.operators[node.a] {
        Equal => Some(NotEqual)
        NotEqual => Some(Equal)
        StrictlyEqual => Some(StrictlyNotEqual)
        StrictlyNotEqual => Some(StrictlyEqual)
        _ => None
      }
      if opposite is Some(operator) {
        return self.node(Binary, self.add_operator(operator), node.b, node.c, 0)
      }
    }
    _ => ()
  }
  -1
}

///|
fn Program::remove_final_unit_return(
  self : Program,
  function_id : Int32,
) -> Unit {
  let function = self.functions[function_id]
  guard (function.flags & FUNCTION_EXPRESSION_BODY) == 0 else { return }
  self.trim_function_tail(function.body)
  let body = self.nodes[function.body]
  guard (function.flags & FUNCTION_ARROW) != 0 &&
    body.kind is Block &&
    body.b == 1 else {
    return
  }
  let result = self.nodes[self.edges[body.a]]
  guard result.kind is Return && result.a >= 0 else { return }
  let value = self.nodes[result.a]
  let expression = if value.kind is (Sequence | ObjectLiteral) {
    self.node(Parenthesized, result.a, 0, 0, 0)
  } else {
    result.a
  }
  self.functions[function_id] = {
    ..function,
    body: expression,
    flags: function.flags | FUNCTION_EXPRESSION_BODY,
  }
}

///|
/// Falling off a function returns undefined. Only follow tails that execute
/// once; a return in a loop, switch or finally has different control flow.
fn Program::trim_function_tail(self : Program, id : Int32) -> Unit {
  let node = self.nodes[id]
  match node.kind {
    Return if node.a < 0 => self.nodes[id] = Node::new(Empty, 0, 0, 0, 0)
    Block if node.b > 0 => {
      let tail = self.edges[node.a + node.b - 1]
      self.trim_function_tail(tail)
      if self.is_empty_statement(tail) {
        self.nodes[id] = { ..node, b: node.b - 1, }
      }
    }
    If => {
      self.trim_function_tail(node.b)
      if node.c >= 0 {
        self.trim_function_tail(node.c)
      }
      let alternative_empty = node.c < 0 || self.is_empty_statement(node.c)
      if self.is_empty_statement(node.b) {
        if alternative_empty {
          let condition = self.node(Parenthesized, node.a, 0, 0, 0)
          self.nodes[id] = Node::new(ExpressionStatement, condition, 0, 0, 0)
        } else {
          let inverted = self.negated_value(node.a)
          self.nodes[id] = Node::new(If, inverted, node.c, -1, 0)
        }
      } else if alternative_empty {
        self.nodes[id] = { ..node, c: -1, }
      }
    }
    _ => ()
  }
}

///|
fn Program::is_empty_statement(self : Program, id : Int32) -> Bool {
  let node = self.nodes[id]
  node.kind is Empty || (node.kind is Block && node.b == 0)
}
