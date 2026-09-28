///|
#warnings("-implicit_impl_as_method")
priv enum Nk {
  Ident
  NumberLit
  StringLit
  RegexLit
  TemplateNoSub
  Template
  TaggedTemplate
  BoolLit
  NullLit
  This
  Super
  ArrayLit
  ObjectLit
  Prop
  PatIdent
  PatArray
  PatObject
  PatProp
  PatRest
  PatDefault
  CoverDefault
  FunctionExpr
  FunctionDecl
  ArrowExpr
  ClassExpr
  ClassDecl
  Unary
  Update
  Binary
  Logical
  Assign
  Cond
  Call
  New
  Member
  Index
  Sequence
  Spread
  Paren
  ExprStmt
  Block
  VarDecl
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
  ModuleStmt
} derive(Eq, Debug)

///|
extend Nk with @debug.Debug::{to_repr}

///|
#warnings("-implicit_impl_as_method")
priv enum Op {
  Assign
  Add
  Sub
  Mul
  Div
  Mod
  Exp
  Shl
  Shr
  Ushr
  Lt
  Gt
  Le
  Ge
  Eq
  Ne
  StrictEq
  StrictNe
  BitAnd
  BitOr
  BitXor
  LogicalAnd
  LogicalOr
  Nullish
  In
  Instanceof
  Not
  BitNot
  Plus
  Minus
  Typeof
  Void
  Delete
  Await
  Yield
  YieldStar
  Inc
  Dec
} derive(Eq)

///|
#valtype
priv struct Node {
  kind : Nk
  a : Int
  b : Int
  c : Int
  d : Int
}

///|
fn Node::new(kind : Nk, a : Int, b : Int, c : Int, d : Int) -> Node {
  { kind, a, b, c, d, }
}

///|
#valtype
priv struct Func {
  name : Int
  params_off : Int
  params_count : Int
  body : Int
  flags : Int
}

///|
let fn_async : Int = 1

///|
let fn_generator : Int = 2

///|
let fn_arrow : Int = 4

///|
let fn_expr_body : Int = 8

///|
#valtype
priv struct Class {
  name : Int
  superclass : Int
  members_off : Int
  members_count : Int
}

///|
#valtype
priv struct Ref {
  scope : Int
  name_start : Int
  name_end : Int
  name : Int
  sym : Int
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
let sym_must_not_rename : Int = 1

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
  src : String
  nodes : Array[Node]
  edges : Array[Int]
  refs : Array[Ref]
  symbols : Array[Symbol]
  scopes : Array[Scope]
  scope_members : Array[Map[Int, Int]]
  funcs : Array[Func]
  classes : Array[Class]
  ops : Array[Op]
  modules : Array[ModuleStatement]
  mut root : Int
  preserved_names : Map[Int, Bool]
  names : Array[String]
  generated_names : Array[String]
  identifier_ids : Map[StringView, Int]
  mut has_direct_eval : Bool
  mut cover_defaults : Int
}

///|
fn Program::new(src : String) -> Program {
  let p : Program = {
    src,
    nodes: [],
    edges: [],
    refs: [],
    symbols: [],
    scopes: [],
    scope_members: [],
    funcs: [],
    classes: [],
    ops: [],
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
    has_direct_eval: false,
    cover_defaults: 0,
  }
  ignore(p.push_scope(-1, scope_module))
  p
}

///|
fn Program::node(
  self : Program,
  kind : Nk,
  a : Int,
  b : Int,
  c : Int,
  d : Int,
) -> Int {
  self.nodes.push(Node::new(kind, a, b, c, d))
  self.nodes.length() - 1
}

///|
fn Program::op(self : Program, t : Op) -> Int {
  self.ops.push(t)
  self.ops.length() - 1
}

///|
fn Program::edge_list(self : Program, xs : Array[Int]) -> (Int, Int) {
  let off = self.edges.length()
  for x in xs {
    self.edges.push(x)
  }
  (off, xs.length())
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
  let spelling = self.src[start:end]
  if self.identifier_ids.get(spelling) is Some(id) {
    return id
  }
  let escaped = spelling.contains_code_unit('\\')
  let name = if escaped {
    identifier_name(self.src, start, end)
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
