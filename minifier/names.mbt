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
const ASCII_CHARACTER_COUNT : Int = 128

///|
priv struct NameGenerator {
  alphabet : NameAlphabet
  reserved : @hashset.HashSet[String]
  mut index : Int
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
  start : Int,
  end : Int,
  counts : FixedArray[Int],
  delta : Int,
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
fn Program::unchanged_character_counts(program : Program) -> FixedArray[Int] {
  let counts = program.character_counts.copy()
  // One bit per UTF-16 source position, shared by references and property keys.
  let counted_removals = FixedArray::make(program.source.length() / 32 + 1, 0U)
  let subtract_name = (start, end) => {
    guard mark_removed_position(counted_removals, start) else { return }
    count_characters(program.source, start, end, counts, -1)
  }
  for reference in program.references {
    guard reference.symbol >= 0 &&
      (program.symbols[reference.symbol].flags & SYMBOL_PRESERVED) == 0 else {
      continue
    }
    subtract_name(reference.name_start, reference.name_end)
  }
  for node in program.nodes {
    guard (node.kind == Labeled || node.kind == Break || node.kind == Continue) &&
      node.a >= 0 else {
      continue
    }
    subtract_name(node.a, node.b)
  }
  for id, _ in program.property_names {
    let node = program.nodes[program.property_leaf(id)]
    subtract_name(node.a, node.b)
  }
  for id, _ in program.member_names {
    let node = program.nodes[id]
    subtract_name(node.b, node.c)
  }
  counts
}

///|
fn mark_removed_position(positions : FixedArray[UInt], start : Int) -> Bool {
  let word_index = start / 32
  let position_mask = 1U << (start % 32)
  let word = positions[word_index]
  guard (word & position_mask) == 0U else { return false }
  positions[word_index] = word | position_mask
  true
}

///|
fn alphabet_by_frequency(counts : FixedArray[Int]) -> NameAlphabet {
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
