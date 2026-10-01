///|
priv struct PropertyAnalysis {
  program : Program
  allowed : @hashset.HashSet[String]
  pinned : @hashset.HashSet[String]
  keys : Map[Int, String]
  members : Map[Int, String]
  literals : @hashset.HashSet[Int]
  class_methods : @hashset.HashSet[Int]
  quoted_names : Map[StringView, String?]
}

///|
fn PropertyAnalysis::new(
  program : Program,
  requested : ArrayView[String],
) -> PropertyAnalysis {
  let allowed = @hashset.HashSet([])
  for name in requested {
    guard name =~ ASCII_PROPERTY_NAME else { continue }
    allowed.add(name)
  }
  let pinned = keywords.copy()
  pinned.add("__proto__")
  pinned.add("constructor")
  pinned.add("prototype")
  {
    program,
    allowed,
    pinned,
    keys: {},
    members: {},
    literals: @hashset.HashSet([]),
    class_methods: program.class_method_ids(),
    quoted_names: {},
  }
}

///|
fn Program::class_method_ids(self : Program) -> @hashset.HashSet[Int] {
  let methods = @hashset.HashSet([])
  for class in self.classes {
    for index in 0..<class.members_count {
      let id = self.edges[class.members_offset + index]
      guard self.nodes[id].kind == Method else { continue }
      methods.add(id)
    }
  }
  methods
}

///|
fn PropertyAnalysis::quoted_name(
  self : PropertyAnalysis,
  start : Int,
  end : Int,
) -> String? {
  let spelling = self.program.source[start:end]
  guard self.quoted_names.get(spelling) is Some(name) else {
    let decoded = quoted_property_name(self.program.source, start, end)
    self.quoted_names[spelling] = decoded
    return decoded
  }
  name
}

///|
fn PropertyAnalysis::key_name(
  self : PropertyAnalysis,
  id : Int,
  computed : Bool,
) -> String? raise ParseError {
  let program = self.program
  let node = program.nodes[property_leaf(program, id)]
  guard node.kind == StringLiteral || node.kind == TemplateNoSubstitution else {
    return None
  }
  let is_quoted = program.source.view(start_offset=node.a) =~ (
    PROPERTY_QUOTE_START,
    after=_,
  )
  guard !is_quoted else { return self.quoted_name(node.a, node.b) }
  guard !computed else { return None }
  let name = program.names[program.identifier(node.a, node.b)]
  guard name =~ ASCII_PROPERTY_NAME else { return None }
  Some(name)
}

///|
fn PropertyAnalysis::collect_key(
  self : PropertyAnalysis,
  id : Int,
  computed : Bool,
) -> Unit raise ParseError {
  guard self.key_name(id, computed) is Some(name) else { return }
  self.keys[id] = name
  self.literals.add(property_leaf(self.program, id))
}

///|
fn PropertyAnalysis::collect(self : PropertyAnalysis) -> Unit raise ParseError {
  let program = self.program
  for id in 0..<program.nodes.length() {
    let node = program.nodes[id]
    match node.kind {
      Member if node.a >= 0 => {
        let name = program.names[program.identifier(node.b, node.c)]
        guard name =~ ASCII_PROPERTY_NAME else { continue }
        self.members[id] = name
      }
      Property | PatternProperty =>
        self.collect_key(node.a, node.d == PROPERTY_COMPUTED)
      Method => {
        let computed = if self.class_methods.contains(id) {
          (node.d & CLASS_MEMBER_COMPUTED) != 0
        } else {
          node.d == PROPERTY_COMPUTED
        }
        self.collect_key(node.a, computed)
      }
      ClassField =>
        self.collect_key(node.a, (node.d & CLASS_MEMBER_COMPUTED) != 0)
      Index => self.collect_key(node.b, true)
      Binary if program.operators[node.a] == Operator::In =>
        self.collect_key(node.b, true)
      _ => ()
    }
  }
}

///|
fn PropertyAnalysis::preserve_strings(self : PropertyAnalysis) -> Unit {
  let program = self.program
  for id in 0..<program.nodes.length() {
    let node = program.nodes[id]
    guard !self.literals.contains(id) &&
      (node.kind == StringLiteral || node.kind == TemplateNoSubstitution) else {
      continue
    }
    guard program.source.view(start_offset=node.a) =~ (
      PROPERTY_QUOTE_START,
      after=_,
    ) else {
      continue
    }
    guard self.quoted_name(node.a, node.b) is Some(name) else { continue }
    self.pinned.add(name)
  }
}

///|
fn PropertyAnalysis::preserve_attributes(self : PropertyAnalysis) -> Unit {
  let program = self.program
  for statement in program.modules {
    let attributes = match statement {
      Import(_, _, attributes)
      | ExportNames(_, _, attributes)
      | ExportAll(_, _, attributes) => attributes
      _ => -1
    }
    guard attributes >= 0 else { continue }
    let object = program.nodes[attributes]
    for index in 0..<object.b {
      let property = program.nodes[program.edges[object.a + index]]
      guard self.keys.get(property.a) is Some(name) else { continue }
      self.pinned.add(name)
    }
  }
}

///|
fn PropertyAnalysis::select(
  self : PropertyAnalysis,
  uses : Map[Int, String],
  selected : Map[Int, String],
) -> Unit {
  for id, name in uses {
    self.program.property_reserved.add(name)
    guard self.allowed.contains(name) && !self.pinned.contains(name) else {
      continue
    }
    selected[id] = name
  }
}

///|
fn prepare_properties(
  program : Program,
  requested : ArrayView[String],
) -> Unit raise ParseError {
  guard requested.length() > 0 && !program.has_dynamic_scope else { return }
  let analysis = PropertyAnalysis::new(program, requested)
  analysis.collect()
  analysis.preserve_strings()
  analysis.preserve_attributes()
  for name in analysis.pinned {
    program.property_reserved.add(name)
  }
  analysis.select(analysis.keys, program.property_names)
  analysis.select(analysis.members, program.member_names)
}

///|
#valtype
priv struct PropertyFrequency {
  name : String
  count : Int
}

///|
fn mangle_properties(program : Program) -> Unit {
  guard !program.property_names.is_empty() || !program.member_names.is_empty() else {
    return
  }
  let groups = [program.property_names, program.member_names]
  let frequencies = property_frequencies(groups)
  let generator = NameGenerator::new(
    program.alphabet,
    program.property_reserved,
  )
  let renamed : Map[String, String] = Map([])
  for property in frequencies {
    let candidate = generator.next_name()
    renamed[property.name] = candidate
    program.property_reserved.add(candidate)
  }
  for uses in groups {
    for id, name in uses {
      uses[id] = renamed.get(name).unwrap()
    }
  }
}

///|
fn property_frequencies(
  groups : Array[Map[Int, String]],
) -> Array[PropertyFrequency] {
  let counts : Map[String, Int] = Map([])
  for uses in groups {
    for _, name in uses {
      counts[name] = counts.get(name).unwrap_or(0) + 1
    }
  }
  let frequencies : Array[PropertyFrequency] = []
  for name, count in counts {
    frequencies.push({ name, count, })
  }
  frequencies.sort_by((left, right) => {
    let difference = right.count - left.count
    guard difference == 0 else { return difference }
    left.name.compare(right.name)
  })
  frequencies
}

///|
fn Printer::property_expression(self : Printer, id : Int) -> Unit {
  guard self.program.property_names.get(id) is Some(name) else {
    self.expression(id)
    return
  }
  self.text("\"", false)
  self.text(name, false)
  self.text("\"", false)
}
