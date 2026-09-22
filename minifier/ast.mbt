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
  Inc
  Dec
} derive(Eq)

///|
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
priv struct Class {
  name : Int
  superclass : Int
  members_off : Int
  members_count : Int
}

///|
priv struct Ref {
  scope : Int
  name_start : Int
  name_end : Int
  mut sym : Int
}

///|
priv struct Symbol {
  name_start : Int
  name_end : Int
  scope : Int
  flags : Int
  mut use_count : Int
  mut new_name : String
}

///|
let sym_must_not_rename : Int = 1

///|
priv struct Scope {
  parent : Int
  kind : Int
  members : Map[String, Int]
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
priv struct Program {
  src : String
  nodes : Array[Node]
  edges : Array[Int]
  refs : Array[Ref]
  symbols : Array[Symbol]
  scopes : Array[Scope]
  funcs : Array[Func]
  classes : Array[Class]
  ops : Array[Op]
  mut root : Int
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
    funcs: [],
    classes: [],
    ops: [],
    root: -1,
  }
  p.scopes.push({ parent: -1, kind: scope_module, members: {}, })
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
  self.scopes.push({ parent, kind, members: {}, })
  self.scopes.length() - 1
}

///|
fn Program::src_span(self : Program, start : Int, end : Int) -> String {
  self.src[start:end].to_owned()
}

///|
fn Program::add_symbol(
  self : Program,
  name_start : Int,
  name_end : Int,
  scope : Int,
) -> Int {
  let name = self.src_span(name_start, name_end)
  match self.scopes[scope].members.get(name) {
    Some(id) => id
    None => {
      self.symbols.push({
        name_start,
        name_end,
        scope,
        flags: 0,
        use_count: 0,
        new_name: "",
      })
      let id = self.symbols.length() - 1
      self.scopes[scope].members[name] = id
      id
    }
  }
}
