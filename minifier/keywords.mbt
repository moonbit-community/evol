///|
priv enum Keyword {
  NotKeyword
  As
  Async
  Await
  Break
  Case
  Catch
  Class
  Const
  Continue
  Debugger
  Default
  Delete
  Do
  Else
  Export
  Extends
  False
  Finally
  For
  From
  Function
  Get
  If
  Import
  In
  InstanceOf
  Let
  New
  Null
  Of
  Return
  Set
  Static
  Super
  Switch
  This
  Throw
  True
  Try
  TypeOf
  Var
  Void
  While
  With
  Yield
} derive(Eq)

///|
fn Keyword::spelling(self : Keyword) -> String {
  match self {
    NotKeyword => "identifier"
    As => "as"
    Async => "async"
    Await => "await"
    Break => "break"
    Case => "case"
    Catch => "catch"
    Class => "class"
    Const => "const"
    Continue => "continue"
    Debugger => "debugger"
    Default => "default"
    Delete => "delete"
    Do => "do"
    Else => "else"
    Export => "export"
    Extends => "extends"
    False => "false"
    Finally => "finally"
    For => "for"
    From => "from"
    Function => "function"
    Get => "get"
    If => "if"
    Import => "import"
    In => "in"
    InstanceOf => "instanceof"
    Let => "let"
    New => "new"
    Null => "null"
    Of => "of"
    Return => "return"
    Set => "set"
    Static => "static"
    Super => "super"
    Switch => "switch"
    This => "this"
    Throw => "throw"
    True => "true"
    Try => "try"
    TypeOf => "typeof"
    Var => "var"
    Void => "void"
    While => "while"
    With => "with"
    Yield => "yield"
  }
}

///|
fn keyword_of(spelling : StringView) -> Keyword {
  match spelling {
    "as" => As
    "async" => Async
    "await" => Await
    "break" => Break
    "case" => Case
    "catch" => Catch
    "class" => Class
    "const" => Const
    "continue" => Continue
    "debugger" => Debugger
    "default" => Default
    "delete" => Delete
    "do" => Do
    "else" => Else
    "export" => Export
    "extends" => Extends
    "false" => False
    "finally" => Finally
    "for" => For
    "from" => From
    "function" => Function
    "get" => Get
    "if" => If
    "import" => Import
    "in" => In
    "instanceof" => InstanceOf
    "let" => Let
    "new" => New
    "null" => Null
    "of" => Of
    "return" => Return
    "set" => Set
    "static" => Static
    "super" => Super
    "switch" => Switch
    "this" => This
    "throw" => Throw
    "true" => True
    "try" => Try
    "typeof" => TypeOf
    "var" => Var
    "void" => Void
    "while" => While
    "with" => With
    "yield" => Yield
    _ => NotKeyword
  }
}
