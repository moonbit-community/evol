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
const DECLARATION_VAR : Int = 0

///|
const DECLARATION_LET : Int = 1

///|
const DECLARATION_CONST : Int = 2

///|
const METHOD_ORDINARY : Int = 0

///|
const METHOD_GETTER : Int = 1

///|
const METHOD_SETTER : Int = 2

///|
const CLASS_MEMBER_STATIC : Int = 1

///|
const CLASS_MEMBER_COMPUTED : Int = 2

///|
const PROPERTY_COMPUTED : Int = 1

///|
const PROPERTY_SHORTHAND : Int = 1

///|
const CHAIN_OPTIONAL : Int = 1

///|
const MEMBER_NEW_TARGET : Int = 2

///|
const UPDATE_PREFIX : Int = 1

///|
const UPDATE_POSTFIX : Int = 0

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
const SYMBOL_PRESERVED : Int = 1

///|
const SYMBOL_ELIDED : Int = 2

///|
#valtype
priv struct Scope {
  parent : Int
  kind : Int
}

///|
const SCOPE_MODULE : Int = 0

///|
const SCOPE_FUNCTION : Int = 1

///|
const SCOPE_BLOCK : Int = 2

///|
const SCOPE_CATCH : Int = 3

///|
const SCOPE_CLASS : Int = 4

///|
const SCOPE_PARAMETERS : Int = 5

///|
const SCOPE_ARROW_PARAMETERS : Int = 6

///|
const SCOPE_TRANSPARENT : Int = 7

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
  identifier_ids : IdentifierTable
  mut has_dynamic_scope : Bool
  mut cover_defaults : Int
  character_counts : FixedArray[Int]
  mut alphabet : NameAlphabet
  property_names : Map[Int, String]
  member_names : Map[Int, String]
  property_reserved : @hashset.HashSet[String]
}

///|
fn Program::new(source : String) -> Program {
  let program : Program = {
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
    identifier_ids: IdentifierTable::new(),
    has_dynamic_scope: false,
    cover_defaults: 0,
    character_counts: FixedArray::make(ASCII_CHARACTER_COUNT, 0),
    alphabet: { first: NAME_FIRST, rest: NAME_REST, },
    property_names: {},
    member_names: {},
    property_reserved: @hashset.HashSet([]),
  }
  ignore(program.push_scope(-1, SCOPE_MODULE))
  program
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
fn Program::add_reference(self : Program, reference : Reference) -> Int {
  self.references.push(reference)
  self.references.length() - 1
}

///|
fn Program::add_operator(self : Program, operator : Operator) -> Int {
  self.operators.push(operator)
  self.operators.length() - 1
}

///|
fn Program::edge_list(self : Program, items : PendingEdges) -> EdgeRange {
  let offset = self.edges.length()
  let count = items.length()
  for item in items.view() {
    self.edges.push(item)
  }
  items.release()
  { offset, count, }
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
  let left_root = self.symbol_root(left)
  let right_root = self.symbol_root(right)
  guard left_root != right_root else { return }
  self.symbols[right_root] = { ..self.symbols[right_root], link: left_root, }
}

///|
const NAME_ARGUMENTS : Int = 0

///|
const NAME_EVAL : Int = 1

///|
const NAME_PROTO : Int = 2

///|
/// Intern source views without copying ordinary identifiers on each reference.
/// Escaped spellings and literal spellings share the same canonical identity.
fn Program::identifier(
  self : Program,
  start : Int,
  end : Int,
) -> Int raise ParseError {
  let spelling = self.source[start:end]
  guard self.identifier_ids.get(spelling) is Some(id) else {
    return self.intern_identifier(spelling)
  }
  id
}

///|
fn Program::intern_identifier(
  self : Program,
  spelling : StringView,
) -> Int raise ParseError {
  let start = spelling.start_offset()
  let end = start + spelling.length()
  let escaped = spelling.contains_code_unit('\\')
  let name = if escaped {
    identifier_name(self.source, start, end)
  } else {
    spelling.to_owned()
  }
  let canonical_id = if escaped {
    self.identifier_ids.get(name[:])
  } else {
    None
  }
  guard canonical_id is Some(id) else {
    let id = self.names.length()
    self.names.push(name)
    self.identifier_ids.insert(spelling, id)
    if escaped {
      self.identifier_ids.insert(name[:], id)
    }
    return id
  }
  self.identifier_ids.insert(spelling, id)
  id
}

///|
#valtype
priv struct EdgeRange {
  offset : Int
  count : Int
}
