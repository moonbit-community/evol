///|
priv struct NameAlphabet {
  first : String
  rest : String
}

///|
const NAME_FIRST : String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$"

///|
const NAME_REST : String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$0123456789"

///|
const ASCII_CHARACTER_COUNT : Int32 = 128

///|
priv struct NameGenerator {
  alphabet : NameAlphabet
  reserved : @hashset.HashSet[String]
  mut index : Int32
}

///|
fn NameGenerator::new(
  alphabet : NameAlphabet,
  reserved : @hashset.HashSet[String],
) -> NameGenerator {
  { alphabet, reserved, index: 0, }
}

///|
fn NameGenerator::next_name(self : NameGenerator) -> String {
  for index = self.index {
    let name = self.alphabet.short_name(index)
    guard !self.reserved.contains(name) else { continue index + 1 }
    self.index = index + 1
    break name
  }
}

///|
/// Count source characters directly; alphabet selection filters identifier chars.
fn count_characters(
  source : String,
  start : Int32,
  end : Int32,
  counts : FixedArray[Int32],
  delta : Int32,
) -> Unit {
  for index in start..<end {
    let character = source[index].to_int()
    guard character < ASCII_CHARACTER_COUNT else { continue }
    counts[character] += delta
  }
}

///|
fn Program::name_alphabet(program : Program) -> NameAlphabet {
  let counts = program.unchanged_character_counts()
  alphabet_by_frequency(counts)
}

///|
fn Program::unchanged_character_counts(program : Program) -> FixedArray[Int32] {
  let counts = program.character_counts.copy()
  // A source token may represent both a shorthand key and a reference.
  // Decide removals first, then count each remaining deferred spelling once.
  let accounted_positions = FixedArray::make(
    program.source.length() / 32 + 1,
    0U,
  )
  let remove_name = (start, end) => {
    guard mark_position(accounted_positions, start) else { return }
    if !deferred_identifier_span(program.source, start, end) {
      count_characters(program.source, start, end, counts, -1)
    }
  }
  for reference in program.references {
    if reference.symbol >= 0 &&
      (program.symbols[reference.symbol].flags & SYMBOL_PRESERVED) == 0 {
      remove_name(reference.name_start, reference.name_end)
    }
  }
  for id, _ in program.property_names {
    let node = program.nodes[program.property_leaf(id)]
    remove_name(node.a, node.b)
  }
  for id, _ in program.member_names {
    let node = program.nodes[id]
    remove_name(node.b, node.c)
  }
  let retain_name = (start, end) => {
    guard mark_position(accounted_positions, start) &&
      deferred_identifier_span(program.source, start, end) else {
      return
    }
    count_characters(program.source, start, end, counts, 1)
  }
  for node in program.nodes {
    match node.kind {
      Labeled | Break | Continue if node.a >= 0 => remove_name(node.a, node.b)
      Member => retain_name(node.b, node.c)
      StringLiteral => retain_name(node.a, node.b)
      _ => ()
    }
  }
  for reference in program.references {
    retain_name(reference.name_start, reference.name_end)
  }
  counts
}

///|
fn deferred_identifier_span(
  source : String,
  start : Int32,
  end : Int32,
) -> Bool {
  guard start < end else { return false }
  match source[start] {
    '\'' | '"' | '`' | '#' | '.' | '0'..<='9' => false
    _ => keyword_of(source[start:end]) is NotKeyword
  }
}

///|
fn mark_position(positions : FixedArray[UInt32], start : Int32) -> Bool {
  let word_index = start / 32
  let position_mask = 1U << (start % 32)
  let word = positions[word_index]
  guard (word & position_mask) == 0U else { return false }
  positions[word_index] = word | position_mask
  true
}

///|
fn alphabet_by_frequency(counts : FixedArray[Int32]) -> NameAlphabet {
  let order = Array::makei(NAME_REST.length(), index => index)
  order.sort_by((left, right) => {
    let difference = counts[NAME_REST[right].to_int()] -
      counts[NAME_REST[left].to_int()]
    guard difference == 0 else { return difference }
    left - right
  })
  let first = StringBuilder()
  let rest = StringBuilder()
  for index in order {
    let character = NAME_REST.get_char(index).unwrap()
    rest.write_char(character)
    guard index < NAME_FIRST.length() else { continue }
    first.write_char(character)
  }
  { first: first.to_string(), rest: rest.to_string(), }
}
