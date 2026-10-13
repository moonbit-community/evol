///|
/// A static data value cannot read, call or throw during initialization.
/// Moving its allocation to its only read therefore preserves object identity.
fn Program::is_static_data_value(self : Program, id : Int32) -> Bool {
  guard id >= 0 else { return false }
  let node = self.nodes[self.unwrapped_expression(id)]
  match node.kind {
    NumberLiteral
    | ZeroLiteral
    | StringLiteral
    | BooleanLiteral
    | NullLiteral => true
    Unary if self.operators[node.a] is Minus =>
      self.nodes[self.unwrapped_expression(node.b)].kind is NumberLiteral
    ArrayLiteral => {
      for index in 0..<node.b {
        let element = self.edges[node.a + index]
        guard element < 0 || self.is_static_data_value(element) else {
          return false
        }
      }
      true
    }
    ObjectLiteral => {
      for index in 0..<node.b {
        let property = self.nodes[self.edges[node.a + index]]
        guard property.kind is Property &&
          property.d != PROPERTY_COMPUTED &&
          property.c != PROPERTY_SHORTHAND &&
          self.is_static_data_value(property.b) else {
          return false
        }
      }
      true
    }
    _ => false
  }
}

///|
/// Only direct top-level statements execute in this frame exactly once.
/// Register candidates after their initializer, so earlier reads keep their TDZ.
fn ValueBindings::inline_data_values(
  self : ValueBindings,
  rename_top_level : Bool,
) -> Unit {
  let program = self.program
  guard rename_top_level && !program.has_dynamic_scope else { return }
  let available = Array::make(program.symbols.length(), -1)
  let root = program.nodes[program.root]
  for index in 0..<root.b {
    let statement_id = program.edges[root.a + index]
    let statement = program.nodes[statement_id]
    match statement.kind {
      VariableDeclaration => {
        for binding_index in 0..<statement.c {
          let binding = program.nodes[program.edges[statement.b + binding_index]]
          self.inline_data_expression(binding.b, available, false)
        }
        guard statement.a == DECLARATION_CONST && statement.c == 1 else {
          continue
        }
        let binding = program.nodes[program.edges[statement.b]]
        let symbol = program.identifier_symbol(binding.a)
        guard symbol >= 0 && !self.has_writes[symbol] else { continue }
        let entry = program.symbols[symbol]
        guard entry.scope == 0 &&
          entry.flags == 0 &&
          entry.link < 0 &&
          entry.use_count == 2 &&
          !program.preserved_names.contains(entry.name) &&
          program.is_static_data_value(binding.b) else {
          continue
        }
        available[symbol] = statement_id
      }
      ExpressionStatement =>
        self.inline_data_expression(statement.a, available, true)
      _ => ()
    }
  }
}

///|
/// Walk expression slots only. Function/class bodies, binding patterns, write
/// targets, delete operands and shorthand keys retain their original bindings.
fn ValueBindings::inline_data_expression(
  self : ValueBindings,
  id : Int32,
  available : Array[Int32],
  needs_primary : Bool,
) -> Unit {
  guard id >= 0 else { return }
  let program = self.program
  let node = program.nodes[id]
  match node.kind {
    Identifier => {
      let reference = program.references[node.a]
      let symbol = reference.symbol
      guard symbol >= 0 && reference.scope == 0 else { return }
      let declaration_id = available[symbol]
      guard declaration_id >= 0 else { return }
      let declaration = program.nodes[declaration_id]
      let binding = program.nodes[program.edges[declaration.b]]
      program.nodes[id] = if needs_primary {
        Node::new(Parenthesized, binding.b, 0, 0, 0)
      } else {
        program.nodes[binding.b]
      }
      let entry = program.symbols[symbol]
      program.symbols[symbol] = { ..entry, flags: entry.flags | SYMBOL_ELIDED, }
      program.nodes[declaration_id] = Node::new(Elided, 0, 0, 0, 0)
      available[symbol] = -1
    }
    Parenthesized => self.inline_data_expression(node.a, available, false)
    EvaluatedValue | Member | Spread =>
      self.inline_data_expression(node.a, available, true)
    Index => {
      self.inline_data_expression(node.a, available, true)
      self.inline_data_expression(node.b, available, false)
    }
    Unary if !(program.operators[node.a] is Delete) =>
      self.inline_data_expression(node.b, available, true)
    Binary | Logical => {
      self.inline_data_expression(node.b, available, true)
      self.inline_data_expression(node.c, available, true)
    }
    Assignment => self.inline_data_expression(node.c, available, false)
    Conditional => {
      self.inline_data_expression(node.a, available, true)
      self.inline_data_expression(node.b, available, false)
      self.inline_data_expression(node.c, available, false)
    }
    Call | New => {
      self.inline_data_expression(node.a, available, true)
      if node.b >= 0 {
        self.inline_data_list(node.b, node.c, available, false)
      }
    }
    ArrayLiteral => self.inline_data_list(node.a, node.b, available, false)
    Sequence => self.inline_data_list(node.a, node.b, available, true)
    ObjectLiteral =>
      for index in 0..<node.b {
        let property = program.nodes[program.edges[node.a + index]]
        guard property.kind is Property &&
          property.d != PROPERTY_COMPUTED &&
          property.c != PROPERTY_SHORTHAND else {
          continue
        }
        self.inline_data_expression(property.b, available, false)
      }
    _ => ()
  }
}

///|
fn ValueBindings::inline_data_list(
  self : ValueBindings,
  offset : Int32,
  count : Int32,
  available : Array[Int32],
  needs_primary : Bool,
) -> Unit {
  for index in 0..<count {
    self.inline_data_expression(
      self.program.edges[offset + index],
      available,
      needs_primary,
    )
  }
}
