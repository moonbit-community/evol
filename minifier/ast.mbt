///|
priv enum NodeKind {
  Identifier
  NumberLiteral
  StringLiteral
  RegexLiteral
  TemplateNoSubstitution
  Template
  TaggedTemplate
  BooleanLiteral
  NullLiteral
  This
  Super
  ArrayLiteral
  ObjectLiteral
  Property
  PatternIdentifier
  PatternArray
  PatternObject
  PatternProperty
  PatternRest
  PatternDefault
  CoverDefault
  FunctionExpression
  FunctionDeclaration
  ArrowExpression
  ClassExpression
  ClassDeclaration
  Unary
  Update
  Binary
  Logical
  Assignment
  Conditional
  Call
  New
  Member
  Index
  Sequence
  Spread
  Parenthesized
  ExpressionStatement
  Block
  VariableDeclaration
  Declarator
  If
  For
  ForIn
  ForOf
  While
  DoWhile
  Return
  Throw
  Break
  Continue
  Labeled
  Switch
  SwitchCase
  Try
  Catch
  Debugger
  Empty
  With
  Method
  ClassField
  ClassStaticBlock
  ModuleStatement
} derive(Eq, Debug)

///|
extend NodeKind with @debug.Debug::{to_repr}

///|
priv enum Operator {
  Assignment
  Add
  Subtract
  Multiply
  Divide
  Modulo
  Exponent
  ShiftLeft
  ShiftRight
  UnsignedShiftRight
  LessThan
  GreaterThan
  LessThanOrEqual
  GreaterThanOrEqual
  Equal
  NotEqual
  StrictlyEqual
  StrictlyNotEqual
  BitwiseAnd
  BitwiseOr
  BitwiseXor
  LogicalAnd
  LogicalOr
  NullishCoalescing
  In
  InstanceOf
  LogicalNot
  BitwiseNot
  Plus
  Minus
  TypeOf
  Void
  Delete
  Await
  Yield
  YieldStar
  Increment
  Decrement
} derive(Eq)

///|
#valtype
priv struct Node {
  kind : NodeKind
  a : Int
  b : Int
  c : Int
  d : Int
}

///|
fn Node::new(kind : NodeKind, a : Int, b : Int, c : Int, d : Int) -> Node {
  { kind, a, b, c, d, }
}

///|
#valtype
priv struct Function {
  name : Int
  parameters_offset : Int
  parameters_count : Int
  body : Int
  flags : Int
}

///|
const FUNCTION_ASYNC : Int = 1

///|
const FUNCTION_GENERATOR : Int = 2

///|
const FUNCTION_ARROW : Int = 4

///|
const FUNCTION_EXPRESSION_BODY : Int = 8

///|
#valtype
priv struct Class {
  name : Int
  superclass : Int
  members_offset : Int
  members_count : Int
}

///|
#valtype
priv struct Reference {
  scope : Int
  name_start : Int
  name_end : Int
  name : Int
  symbol : Int
}

///|
#valtype
priv struct Symbol {
  name : Int
  scope : Int
  flags : Int
  link : Int
  use_count : Int
  name_slot : Int
}

///|
let symbol_preserved : Int = 1

///|
#valtype
priv struct Scope {
  parent : Int
  kind : Int
}

///|
let scope_module : Int = 0

///|
let scope_function : Int = 1

///|
let scope_block : Int = 2

///|
let scope_catch : Int = 3

///|
let scope_class : Int = 4

///|
let scope_parameters : Int = 5

///|
let scope_arrow_parameters : Int = 6

///|
let scope_transparent : Int = 7

///|
priv struct Program {
  source : String
  nodes : Array[Node]
  edges : Array[Int]
  references : Array[Reference]
  symbols : Array[Symbol]
  scopes : Array[Scope]
  scope_members : Array[Map[Int, Int]]
  functions : Array[Function]
  classes : Array[Class]
  operators : Array[Operator]
  modules : Array[ModuleStatement]
  mut root : Int
  preserved_names : Map[Int, Bool]
  names : Array[String]
  generated_names : Array[String]
  identifier_ids : Map[StringView, Int]
  mut has_dynamic_scope : Bool
  mut cover_defaults : Int
  character_counts : Array[Int]
  mut alphabet : NameAlphabet
  property_names : Map[Int, String]
  member_names : Map[Int, String]
  property_reserved : @hashset.HashSet[String]
}

///|
fn Program::new(source : String) -> Program {
  let p : Program = {
    source,
    nodes: [],
    edges: [],
    references: [],
    symbols: [],
    scopes: [],
    scope_members: [],
    functions: [],
    classes: [],
    operators: [],
    modules: [],
    root: -1,
    preserved_names: {},
    names: ["arguments", "eval", "__proto__"],
    generated_names: [],
    identifier_ids: {
      "arguments": name_arguments,
      "eval": name_eval,
      "__proto__": name_proto,
    },
    has_dynamic_scope: false,
    cover_defaults: 0,
    character_counts: Array::make(64, 0),
    alphabet: { first: NAME_FIRST, rest: NAME_REST, },
    property_names: {},
    member_names: {},
    property_reserved: @hashset.HashSet([]),
  }
  ignore(p.push_scope(-1, scope_module))
  p
}

///|
fn Program::node(
  self : Program,
  kind : NodeKind,
  a : Int,
  b : Int,
  c : Int,
  d : Int,
) -> Int {
  self.nodes.push(Node::new(kind, a, b, c, d))
  self.nodes.length() - 1
}

///|
fn Program::add_operator(self : Program, t : Operator) -> Int {
  self.operators.push(t)
  self.operators.length() - 1
}

///|
fn Program::edge_list(self : Program, items : Array[Int]) -> (Int, Int) {
  let offset = self.edges.length()
  for x in items {
    self.edges.push(x)
  }
  (offset, items.length())
}

///|
fn Program::push_scope(self : Program, parent : Int, kind : Int) -> Int {
  self.scope_members.push(Map([]))
  self.scopes.push({ parent, kind, })
  self.scopes.length() - 1
}

///|
fn Program::add_symbol(
  self : Program,
  name_start : Int,
  name_end : Int,
  scope : Int,
) -> Int raise ParseError {
  let name = self.identifier(name_start, name_end)
  guard self.scope_members[scope].get(name) is Some(id) else {
    let id = self.symbols.length()
    self.symbols.push({
      name,
      scope,
      flags: 0,
      link: -1,
      use_count: 0,
      name_slot: -1,
    })
    self.scope_members[scope][name] = id
    return id
  }
  id
}

///|
/// Symbols linked here must keep the same spelling even when JavaScript creates
/// distinct bindings (for example, a catch parameter and a hoisted var).
fn Program::symbol_root(self : Program, id : Int) -> Int {
  for current = id {
    let next = self.symbols[current].link
    guard next >= 0 else { break current }
    continue next
  }
}

///|
fn Program::link_symbols(self : Program, left : Int, right : Int) -> Unit {
  let left = self.symbol_root(left)
  let right = self.symbol_root(right)
  if left != right {
    self.symbols[right] = { ..self.symbols[right], link: left, }
  }
}

///|
let name_arguments : Int = 0

///|
let name_eval : Int = 1

///|
let name_proto : Int = 2

///|
/// Intern source views without copying ordinary identifiers on each reference.
/// Escaped spellings and literal spellings share the same canonical identity.
fn Program::identifier(
  self : Program,
  start : Int,
  end : Int,
) -> Int raise ParseError {
  let spelling = self.source[start:end]
  if self.identifier_ids.get(spelling) is Some(id) {
    return id
  }
  let escaped = spelling.contains_code_unit('\\')
  let name = if escaped {
    identifier_name(self.source, start, end)
  } else {
    spelling.to_owned()
  }
  if escaped && self.identifier_ids.get(name[:]) is Some(id) {
    self.identifier_ids[spelling] = id
    return id
  }
  let id = self.names.length()
  self.names.push(name)
  self.identifier_ids[spelling] = id
  if escaped {
    self.identifier_ids[name[:]] = id
  }
  id
}
