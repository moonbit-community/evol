///|
priv struct PooledLiteral {
  value : Node
  first_occurrence : LiteralOccurrence
  mut further_occurrences : Array[LiteralOccurrence]?
}

///|
priv enum LiteralLocationKind {
  NodeA
  NodeB
  NodeC
  Edge
}

///|
#valtype
priv struct LiteralLocation {
  kind : LiteralLocationKind
  parent_node : Int32
  edge_index : Int32
}

///|
fn LiteralLocation::node_a(parent_node : Int32) -> LiteralLocation {
  { kind: NodeA, parent_node, edge_index: -1, }
}

///|
fn LiteralLocation::node_b(parent_node : Int32) -> LiteralLocation {
  { kind: NodeB, parent_node, edge_index: -1, }
}

///|
fn LiteralLocation::node_c(parent_node : Int32) -> LiteralLocation {
  { kind: NodeC, parent_node, edge_index: -1, }
}

///|
fn LiteralLocation::edge(edge_index : Int32) -> LiteralLocation {
  { kind: Edge, parent_node: -1, edge_index, }
}

///|
#valtype
priv struct LiteralOccurrence {
  literal_node : Int32
  location : LiteralLocation
  scope : Int32
}

///|
priv struct LiteralPool {
  program : Program
  by_spelling : Map[StringView, Int32]
  values : Array[PooledLiteral]
  include_function_bodies : Bool
}

///|
/// Small literals do not repay a new binding. A substantial saving also keeps
/// the extra symbol and reference work focused on repeated static data.
const MIN_POOLED_LITERAL_LENGTH : Int32 = 8

///|
const MIN_LITERAL_POOL_SAVING : Int32 = 64

///|
/// Run after compaction, before mangling. Modules retain root-only pooling:
/// a cyclic importer can call an exported function before its module executes.
fn Program::pool_literals(self : Program, rename_top_level : Bool) -> Unit {
  guard rename_top_level && !self.has_dynamic_scope else { return }
  let pool : LiteralPool = {
    program: self,
    by_spelling: {},
    values: [],
    include_function_bodies: self.modules.length() == 0,
  }
  pool.collect_root()
  let declarations = pool.create_bindings()
  guard declarations.length() > 0 else { return }
  self.insert_literal_pool_declarations(declarations)
}

///|
fn LiteralPool::collect_root(self : LiteralPool) -> Unit {
  let program = self.program
  let root = program.nodes[program.root]
  if self.include_function_bodies {
    self.collect_statements(root.a, root.b, 0)
    return
  }
  for index in 0..<root.b {
    let statement_id = program.edges[root.a + index]
    let statement = program.nodes[statement_id]
    if statement.kind is (VariableDeclaration | ExpressionStatement) {
      self.collect_statement(statement_id, 0)
    }
  }
}

///|
fn LiteralPool::collect_statements(
  self : LiteralPool,
  offset : Int32,
  count : Int32,
  scope : Int32,
) -> Unit {
  for index in 0..<count {
    self.collect_statement(self.program.edges[offset + index], scope)
  }
}

///|
fn LiteralPool::collect_statement(
  self : LiteralPool,
  id : Int32,
  scope : Int32,
) -> Unit {
  guard id >= 0 else { return }
  let program = self.program
  let node = program.nodes[id]
  match node.kind {
    Block if node.d > 0 =>
      self.collect_statements(node.a, node.b, program.reference_scope(node.c))
    VariableDeclaration =>
      for index in 0..<node.c {
        let binding_id = program.edges[node.b + index]
        let binding = program.nodes[binding_id]
        self.collect_expression(
          binding.b,
          LiteralLocation::node_b(binding_id),
          scope,
        )
      }
    ExpressionStatement => {
      // Retain directive strings, including non-prologue string statements.
      guard !(program.nodes[node.a].kind is StringLiteral) else { return }
      self.collect_expression(node.a, LiteralLocation::node_a(id), scope)
    }
    Return | Throw =>
      self.collect_expression(node.a, LiteralLocation::node_a(id), scope)
    If => {
      self.collect_expression(node.a, LiteralLocation::node_a(id), scope)
      self.collect_statement(node.b, scope)
      self.collect_statement(node.c, scope)
    }
    While => {
      self.collect_expression(node.a, LiteralLocation::node_a(id), scope)
      self.collect_statement(node.b, scope)
    }
    DoWhile => {
      self.collect_statement(node.a, scope)
      self.collect_expression(node.b, LiteralLocation::node_b(id), scope)
    }
    For => self.collect_scoped_block(node.d)
    ForIn | ForOf => self.collect_scoped_block(node.c)
    Switch => {
      self.collect_expression(node.a, LiteralLocation::node_a(id), scope)
      let case_scope = program.reference_scope(node.d)
      for index in 0..<node.c {
        let case_id = program.edges[node.b + index]
        let clause = program.nodes[case_id]
        self.collect_expression(
          clause.a,
          LiteralLocation::node_a(case_id),
          case_scope,
        )
        self.collect_statements(clause.b, clause.c, case_scope)
      }
    }
    Try => {
      self.collect_statement(node.a, scope)
      self.collect_statement(node.b, scope)
      self.collect_statement(node.c, scope)
    }
    Catch => self.collect_scoped_block(node.b)
    Labeled => self.collect_statement(node.c, scope)
    FunctionDeclaration => self.collect_function(node.a)
    _ => ()
  }
}

///|
/// A loop header has its own lexical environment. Until that environment has
/// an AST scope field, visit only bodies whose explicit block records it.
fn LiteralPool::collect_scoped_block(self : LiteralPool, id : Int32) -> Unit {
  let body = self.program.nodes[id]
  // A value introduced by later compaction may be missed. This is only a
  // profitability shortcut; leaving its literal in place preserves behavior.
  guard body.kind is Block && body.d > 0 else { return }
  self.collect_statements(body.a, body.b, self.program.reference_scope(body.c))
}

///|
fn LiteralPool::collect_function(self : LiteralPool, id : Int32) -> Unit {
  guard self.include_function_bodies else { return }
  let function = self.program.functions[id]
  guard (function.flags & FUNCTION_EXPRESSION_BODY) == 0 else { return }
  self.collect_scoped_block(function.body)
}

///|
/// Bindings, property names, parameter defaults, classes and templates are
/// never traversed. The recorded scope belongs to the current use location,
/// even when compaction moved a literal from another function or block.
fn LiteralPool::collect_expression(
  self : LiteralPool,
  id : Int32,
  location : LiteralLocation,
  scope : Int32,
) -> Unit {
  guard id >= 0 else { return }
  let program = self.program
  let node = program.nodes[id]
  match node.kind {
    NumberLiteral | StringLiteral => self.collect_literal(id, location, scope)
    Parenthesized | EvaluatedValue | Member | Spread =>
      self.collect_expression(node.a, LiteralLocation::node_a(id), scope)
    Index => self.collect_expression(node.a, LiteralLocation::node_a(id), scope)
    Unary if !(program.operators[node.a] is Delete) =>
      self.collect_expression(node.b, LiteralLocation::node_b(id), scope)
    Binary if program.operators[node.a] is In =>
      self.collect_expression(node.c, LiteralLocation::node_c(id), scope)
    Binary | Logical => {
      self.collect_expression(node.b, LiteralLocation::node_b(id), scope)
      self.collect_expression(node.c, LiteralLocation::node_c(id), scope)
    }
    Assignment =>
      self.collect_expression(node.c, LiteralLocation::node_c(id), scope)
    Conditional => {
      self.collect_expression(node.a, LiteralLocation::node_a(id), scope)
      self.collect_expression(node.b, LiteralLocation::node_b(id), scope)
      self.collect_expression(node.c, LiteralLocation::node_c(id), scope)
    }
    Call | New => {
      let callee_node = program.nodes[program.unwrapped_expression(node.a)]
      if callee_node.kind is Identifier &&
        program.names[program.references[callee_node.a].name] == "import" {
        return
      }
      self.collect_expression(node.a, LiteralLocation::node_a(id), scope)
      if node.b >= 0 {
        self.collect_list(node.b, node.c, scope)
      }
    }
    ArrayLiteral | Sequence => self.collect_list(node.a, node.b, scope)
    ObjectLiteral =>
      for index in 0..<node.b {
        let property_id = program.edges[node.a + index]
        let property = program.nodes[property_id]
        guard property.kind is Property &&
          property.d != PROPERTY_COMPUTED &&
          property.c != PROPERTY_SHORTHAND else {
          continue
        }
        self.collect_expression(
          property.b,
          LiteralLocation::node_b(property_id),
          scope,
        )
      }
    FunctionExpression | ArrowExpression => self.collect_function(node.a)
    _ => ()
  }
}

///|
fn LiteralPool::collect_list(
  self : LiteralPool,
  offset : Int32,
  count : Int32,
  scope : Int32,
) -> Unit {
  for index in 0..<count {
    let position = offset + index
    self.collect_expression(
      self.program.edges[position],
      LiteralLocation::edge(position),
      scope,
    )
  }
}

///|
fn LiteralPool::collect_literal(
  self : LiteralPool,
  id : Int32,
  location : LiteralLocation,
  scope : Int32,
) -> Unit {
  let program = self.program
  let value = program.nodes[id]
  guard value.b - value.a >= MIN_POOLED_LITERAL_LENGTH else { return }
  let occurrence : LiteralOccurrence = { literal_node: id, location, scope, }
  let spelling = program.source[value.a:value.b]
  if self.by_spelling.get(spelling) is Some(index) {
    let pooled = self.values[index]
    match pooled.further_occurrences {
      Some(occurrences) => occurrences.push(occurrence)
      None => pooled.further_occurrences = Some([occurrence])
    }
    return
  }
  self.by_spelling[spelling] = self.values.length()
  self.values.push({
    value,
    first_occurrence: occurrence,
    further_occurrences: None,
  })
}

///|
fn LiteralLocation::replace(
  self : LiteralLocation,
  program : Program,
  replacement : Int32,
) -> Unit {
  match self.kind {
    NodeA => {
      let node = program.nodes[self.parent_node]
      program.nodes[self.parent_node] = { ..node, a: replacement, }
    }
    NodeB => {
      let node = program.nodes[self.parent_node]
      program.nodes[self.parent_node] = { ..node, b: replacement, }
    }
    NodeC => {
      let node = program.nodes[self.parent_node]
      program.nodes[self.parent_node] = { ..node, c: replacement, }
    }
    Edge => program.edges[self.edge_index] = replacement
  }
}

///|
/// Replace recorded uses directly; never overwrite the original literal node.
fn LiteralPool::replace_occurrences(
  self : LiteralPool,
  pooled : PooledLiteral,
  symbol : Int32,
) -> Unit {
  let program = self.program
  let name = program.symbols[symbol].name
  program.replace_literal_occurrence(pooled.first_occurrence, name, symbol)
  if pooled.further_occurrences is Some(occurrences) {
    for occurrence in occurrences {
      program.replace_literal_occurrence(occurrence, name, symbol)
    }
  }
}

///|
fn Program::replace_literal_occurrence(
  self : Program,
  occurrence : LiteralOccurrence,
  name : Int32,
  symbol : Int32,
) -> Unit {
  let value = self.nodes[occurrence.literal_node]
  let reference = self.add_reference({
    scope: occurrence.scope,
    name_start: value.a,
    name_end: value.b,
    name,
    symbol,
  })
  let replacement = self.node(Identifier, reference, 0, 0, 0)
  occurrence.location.replace(self, replacement)
}

///|
fn Program::pooled_literal_length(self : Program, value : Node) -> Int32 {
  let length = value.b - value.a
  if value.kind is StringLiteral {
    let costs = quote_costs(self.source, value.a, value.b)
    let quote = costs.preferred_quote(self.source[value.a])
    let shortened = costs.quoted_length(quote)
    return if shortened < length { shortened } else { length }
  }
  guard decimal_spelling(self.source, value.a, value.b) is Some(decimal) else {
    return length
  }
  if length >= 16 &&
    !decimal.is_integer &&
    shortened_decimal_value(self.source[value.a:value.b]) is Some(shortened) {
    return shortened.length()
  }
  if decimal.exponent_marker == value.b {
    if decimal.is_integer &&
      integer_exponent_spelling(self.source, value.a, value.b) is Some(text) {
      return text.length()
    }
    return value.b - decimal.mantissa_start
  }
  if shortened_exponent_spelling(self.source, value.a, value.b, decimal)
    is Some(text) {
    return text.length()
  }
  length
}

///|
fn LiteralPool::create_bindings(self : LiteralPool) -> Array[Int32] {
  let program = self.program
  // This overestimates the number of generated slots, including every source
  // name as potentially reserved. Actual pool names can only be shorter.
  let slot_bound = program.symbols.length() +
    program.names.length() +
    self.values.length() +
    keywords.length()
  let name_length = program.alphabet.short_name(slot_bound).length()
  let declarations : Array[Int32] = []
  for pooled in self.values {
    guard pooled.further_occurrences is Some(occurrences) else { continue }
    let occurrence_count = occurrences.length() + 1
    // Printing never increases the source length. For repeated values, using
    // that length bounds the saving before quote or numeric conversion work.
    let source_length = pooled.value.b - pooled.value.a
    let declaration_length_bound = source_length +
      name_length +
      "const =;".length()
    let saving_upper_bound = occurrence_count * (source_length - name_length) -
      declaration_length_bound
    guard saving_upper_bound >= MIN_LITERAL_POOL_SAVING else { continue }
    let printed_length = program.pooled_literal_length(pooled.value)
    let declaration_length = printed_length + name_length + "const =;".length()
    let saved = occurrence_count * (printed_length - name_length) -
      declaration_length
    guard saved >= MIN_LITERAL_POOL_SAVING else { continue }
    let name = program.fresh_literal_pool_name()
    let symbol = program.symbols.length()
    program.symbols.push({
      name,
      scope: 0,
      flags: 0,
      link: -1,
      use_count: occurrence_count + 1,
      name_slot: -1,
    })
    program.scope_members[0][name] = symbol
    let value = pooled.value
    let reference = program.add_reference({
      scope: 0,
      name_start: value.a,
      name_end: value.b,
      name,
      symbol,
    })
    let pattern = program.node(PatternIdentifier, reference, 0, 0, 0)
    let initializer = program.node(
      value.kind,
      value.a,
      value.b,
      value.c,
      value.d,
    )
    let binding = program.node(Declarator, pattern, initializer, 0, 0)
    let offset = program.edges.length()
    program.edges.push(binding)
    declarations.push(
      program.node(VariableDeclaration, DECLARATION_CONST, offset, 1, 0),
    )
    self.replace_occurrences(pooled, symbol)
    // Synthetic references remove each replaced source spelling in the usual
    // name histogram pass. Keep one extra copy for this new initializer.
    count_characters(
      program.source,
      value.a,
      value.b,
      program.character_counts,
      1,
    )
  }
  declarations
}

///|
fn Program::fresh_literal_pool_name(self : Program) -> Int32 {
  let spelling = for suffix = self.names.length() {
    let candidate = "$evol_literal_\{suffix}"
    guard self.identifier_ids.get(candidate[:]) is None else {
      continue suffix + 1
    }
    break candidate
  }
  let name = self.names.length()
  self.names.push(spelling)
  self.identifier_ids.insert(spelling[:], name)
  name
}

///|
fn Program::insert_literal_pool_declarations(
  self : Program,
  declarations : Array[Int32],
) -> Unit {
  let root = self.nodes[self.root]
  let directive_count = for index = 0 {
    guard index < root.b else { break index }
    let statement = self.nodes[self.edges[root.a + index]]
    guard statement.kind is ExpressionStatement &&
      self.nodes[statement.a].kind is StringLiteral else {
      break index
    }
    continue index + 1
  }
  let offset = self.edges.length()
  for index in 0..<directive_count {
    self.edges.push(self.edges[root.a + index])
  }
  for declaration in declarations {
    self.edges.push(declaration)
  }
  for index in directive_count..<root.b {
    self.edges.push(self.edges[root.a + index])
  }
  self.nodes[self.root] = {
    ..root,
    a: offset,
    b: root.b + declarations.length(),
  }
}
