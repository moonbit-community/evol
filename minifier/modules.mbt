///|
priv enum ImportBinding {
  Default(Int)
  Namespace(Int)
  // Original imported name node and local binding reference.
  Named(Int, Int)
}

///|
priv enum ModuleStatement {
  Import(Array[ImportBinding], Int, Int)
  // Local expression/original re-export name, followed by the public name.
  ExportNames(Array[(Int, Int)], Int, Int)
  ExportAll(Int, Int, Int)
  ExportDeclaration(Int)
  ExportDefault(Int)
}

///|
fn Parser::module_node(self : Parser, statement : ModuleStatement) -> Int {
  let index = self.program.modules.length()
  self.program.modules.push(statement)
  self.program.node(ModuleStatement, index, 0, 0, 0)
}

///|
fn Parser::expect_keyword(
  self : Parser,
  keyword : Keyword,
) -> Unit raise ParseError {
  guard self.lexer.keyword == keyword else {
    raise self.unexpected(keyword.spelling())
  }
  self.advance()
}

///|
fn Parser::module_name(self : Parser) -> Int raise ParseError {
  guard self.lexer.token == Identifier || self.lexer.token == StringLiteral else {
    raise self.unexpected("module name")
  }
  self.property_key()
}

///|
fn Parser::import_binding(self : Parser) -> Int raise ParseError {
  guard self.lexer.token == Identifier else {
    raise self.unexpected("import binding")
  }
  let reference = self.declare_symbol(0, self.lexer.start, self.lexer.end)
  self.advance()
  reference
}

///|
fn Parser::module_source(self : Parser) -> Int raise ParseError {
  guard self.lexer.token == StringLiteral else {
    raise self.unexpected("module source string")
  }
  self.property_key()
}

///|
fn Parser::import_attributes(self : Parser) -> Int raise ParseError {
  guard self.lexer.keyword == Keyword::With else { return -1 }
  self.advance()
  self.object_literal()
}

///|
fn Parser::parse_import(self : Parser) -> Int raise ParseError {
  guard self.scope == 0 else {
    raise self.syntax_error("import must be at module top level")
  }
  self.expect_keyword(Keyword::Import)
  let bindings = if self.lexer.token == StringLiteral {
    []
  } else {
    self.import_bindings()
  }
  let source = self.module_source()
  let attributes = self.import_attributes()
  self.eat_semicolon()
  self.module_node(Import(bindings, source, attributes))
}

///|
fn Parser::import_bindings(
  self : Parser,
) -> Array[ImportBinding] raise ParseError {
  let bindings : Array[ImportBinding] = []
  let has_default = self.lexer.token == Identifier
  if has_default {
    bindings.push(Default(self.import_binding()))
  }
  let has_more = !has_default || self.eat(Comma)
  if has_more {
    if self.eat(Asterisk) {
      self.expect_keyword(Keyword::As)
      bindings.push(Namespace(self.import_binding()))
    } else {
      self.named_import_bindings(bindings)
    }
  }
  self.expect_keyword(Keyword::From)
  bindings
}

///|
fn Parser::named_import_bindings(
  self : Parser,
  bindings : Array[ImportBinding],
) -> Unit raise ParseError {
  self.expect(LeftBrace)
  while self.lexer.token != RightBrace {
    let imported = self.module_name()
    let reference = if self.lexer.keyword == Keyword::As {
      self.advance()
      self.import_binding()
    } else {
      let name = self.program.nodes[imported]
      let first_character = self.program.source[name.a]
      guard first_character != '\'' && first_character != '"' else {
        raise self.syntax_error("string import name requires a local alias")
      }
      self.declare_symbol(0, name.a, name.b)
    }
    bindings.push(Named(imported, reference))
    guard self.eat(Comma) else { break }
  }
  self.expect(RightBrace)
}

///|
fn Parser::parse_export(self : Parser) -> Int raise ParseError {
  guard self.scope == 0 else {
    raise self.syntax_error("export must be at module top level")
  }
  self.expect_keyword(Keyword::Export)
  let statement = match self.lexer.token {
    _ if self.lexer.keyword == Keyword::Default => self.default_export()
    Asterisk => self.namespace_export()
    LeftBrace => self.named_export()
    _ => self.export_declaration()
  }
  self.module_node(statement)
}

///|
fn Parser::default_export(self : Parser) -> ModuleStatement raise ParseError {
  self.advance()
  let is_declaration = self.lexer.keyword == Keyword::Function ||
    self.lexer.keyword == Keyword::Class ||
    (
      self.lexer.keyword == Keyword::Async &&
      self.peek_is_keyword(Keyword::Function)
    )
  guard !is_declaration else { return ExportDefault(self.statement()) }
  let body = self.assignment_with_in()
  self.eat_semicolon()
  ExportDefault(body)
}

///|
fn Parser::namespace_export(self : Parser) -> ModuleStatement raise ParseError {
  self.advance()
  let export_name = if self.lexer.keyword == Keyword::As {
    self.advance()
    self.module_name()
  } else {
    -1
  }
  self.expect_keyword(Keyword::From)
  let source = self.module_source()
  let attributes = self.import_attributes()
  self.eat_semicolon()
  ExportAll(export_name, source, attributes)
}

///|
fn Parser::named_export(self : Parser) -> ModuleStatement raise ParseError {
  self.advance()
  let names : Array[(Int, Int)] = []
  while self.lexer.token != RightBrace {
    let source_name = self.module_name()
    let exported = if self.lexer.keyword == Keyword::As {
      self.advance()
      self.module_name()
    } else {
      source_name
    }
    names.push((source_name, exported))
    guard self.eat(Comma) else { break }
  }
  self.expect(RightBrace)
  let source = if self.lexer.keyword == Keyword::From {
    self.advance()
    self.module_source()
  } else {
    -1
  }
  let specifiers = names.map(pair => {
    let (source_name, exported) = pair
    guard source < 0 else { return pair }
    let name = self.program.nodes[source_name]
    let reference = self.add_reference(0, name.a, name.b)
    (self.program.node(Identifier, reference, 0, 0, 0), exported)
  })
  let attributes = if source >= 0 { self.import_attributes() } else { -1 }
  self.eat_semicolon()
  ExportNames(specifiers, source, attributes)
}

///|
fn Parser::export_declaration(
  self : Parser,
) -> ModuleStatement raise ParseError {
  let body = self.statement()
  let declaration_kind = self.program.nodes[body].kind
  guard declaration_kind == VariableDeclaration ||
    declaration_kind == FunctionDeclaration ||
    declaration_kind == ClassDeclaration else {
    raise self.unexpected("export declaration")
  }
  self.preserve_exported_names(body)
  ExportDeclaration(body)
}

///|
/// Direct export declarations expose their binding spelling as the public name.
fn Parser::preserve_exported_names(self : Parser, id : Int) -> Unit {
  let node = self.program.nodes[id]
  match node.kind {
    PatternIdentifier => {
      let symbol = self.program.references[node.a].symbol
      self.program.symbols[symbol] = {
        ..self.program.symbols[symbol],
        flags: SYMBOL_PRESERVED,
      }
    }
    PatternArray | PatternObject =>
      for index in 0..<node.b {
        let item = self.program.edges[node.a + index]
        guard item >= 0 else { continue }
        self.preserve_exported_names(item)
      }
    PatternProperty => self.preserve_exported_names(node.b)
    PatternRest | PatternDefault | Declarator =>
      self.preserve_exported_names(node.a)
    VariableDeclaration =>
      for index in 0..<node.c {
        self.preserve_exported_names(self.program.edges[node.b + index])
      }
    FunctionDeclaration | ClassDeclaration => {
      let reference = if node.kind == FunctionDeclaration {
        self.program.functions[node.a].name
      } else {
        self.program.classes[node.a].name
      }
      guard reference >= 0 else { return }
      let symbol = self.program.references[reference].symbol
      self.program.symbols[symbol] = {
        ..self.program.symbols[symbol],
        flags: SYMBOL_PRESERVED,
      }
    }
    _ => ()
  }
}

///|
fn Printer::module_source(
  self : Printer,
  source : Int,
  attributes : Int,
) -> Unit {
  self.expression(source)
  guard attributes >= 0 else { return }
  self.text("with", true)
  self.expression(attributes)
}

///|
fn Printer::module_stmt(self : Printer, index : Int) -> Unit {
  match self.program.modules[index] {
    Import(bindings, source, attributes) =>
      self.import_statement(bindings, source, attributes)
    ExportNames(specifiers, source, attributes) =>
      self.named_export_statement(specifiers, source, attributes)
    ExportAll(export_name, source, attributes) =>
      self.namespace_export_statement(export_name, source, attributes)
    ExportDeclaration(body) => {
      self.text("export", true)
      self.statement(body)
    }
    ExportDefault(body) => {
      self.text("export default", true)
      self.expression(body)
      self.text(";", false)
    }
  }
}

///|
fn Printer::import_statement(
  self : Printer,
  bindings : Array[ImportBinding],
  source : Int,
  attributes : Int,
) -> Unit {
  self.text("import", true)
  self.import_bindings(bindings)
  if bindings.length() > 0 {
    self.text("from", true)
  }
  self.module_source(source, attributes)
  self.text(";", false)
}

///|
fn Printer::import_bindings(
  self : Printer,
  bindings : Array[ImportBinding],
) -> Unit {
  let mut has_default_binding = false
  let mut named_group_open = false
  for binding in bindings {
    match binding {
      Default(reference) => {
        self.reference(reference)
        has_default_binding = true
      }
      Namespace(reference) => {
        if has_default_binding {
          self.text(",", false)
        }
        self.text("*", false)
        self.text("as", true)
        self.reference(reference)
      }
      Named(imported, reference) => {
        if named_group_open || has_default_binding {
          self.text(",", false)
        }
        if !named_group_open {
          self.text("{", false)
          named_group_open = true
        }
        self.expression(imported)
        self.text("as", true)
        self.reference(reference)
      }
    }
  }
  guard named_group_open else { return }
  self.text("}", false)
}

///|
fn Printer::named_export_statement(
  self : Printer,
  specifiers : Array[(Int, Int)],
  source : Int,
  attributes : Int,
) -> Unit {
  self.text("export", true)
  self.text("{", false)
  for index in 0..<specifiers.length() {
    if index > 0 {
      self.text(",", false)
    }
    let (source_name, exported) = specifiers[index]
    self.expression(source_name)
    self.text("as", true)
    self.expression(exported)
  }
  self.text("}", false)
  if source >= 0 {
    self.text("from", true)
    self.module_source(source, attributes)
  }
  self.text(";", false)
}

///|
fn Printer::namespace_export_statement(
  self : Printer,
  export_name : Int,
  source : Int,
  attributes : Int,
) -> Unit {
  self.text("export", true)
  self.text("*", false)
  if export_name >= 0 {
    self.text("as", true)
    self.expression(export_name)
  }
  self.text("from", true)
  self.module_source(source, attributes)
  self.text(";", false)
}
