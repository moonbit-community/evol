///|
#valtype
priv struct IdentifierEntry {
  spelling : StringView
  hash : Int
  id : Int
}

///|
/// Append-only interning table. Slots index dense entries; -1 marks an empty
/// slot. Capacity is a power of two, with at least one quarter kept empty.
priv struct IdentifierTable {
  entries : Array[IdentifierEntry]
  mut slots : Array[Int]
}

///|
fn IdentifierTable::new() -> IdentifierTable {
  let table : IdentifierTable = { entries: [], slots: Array::make(16, -1), }
  table.insert("arguments", name_arguments)
  table.insert("eval", name_eval)
  table.insert("__proto__", name_proto)
  table
}

///|
/// FNV-1a over UTF-16 code units. Equality still compares the full spelling.
fn identifier_hash(spelling : StringView) -> Int {
  let fnv_offset : UInt = 2166136261U
  let fnv_prime : UInt = 16777619U
  let mut hash = fnv_offset
  for index in 0..<spelling.length() {
    hash = (hash ^ spelling[index].to_uint()) * fnv_prime
  }
  hash.reinterpret_as_int()
}

///|
fn IdentifierTable::find_slot(
  self : IdentifierTable,
  spelling : StringView,
  hash : Int,
) -> Int {
  let mask = self.slots.length() - 1
  for slot = hash & mask {
    let index = self.slots[slot]
    guard index >= 0 else { break slot }
    let entry = self.entries[index]
    guard entry.hash == hash && entry.spelling == spelling else {
      continue (slot + 1) & mask
    }
    break slot
  }
}

///|
fn IdentifierTable::get(self : IdentifierTable, spelling : StringView) -> Int? {
  let slot = self.find_slot(spelling, identifier_hash(spelling))
  let index = self.slots[slot]
  guard index >= 0 else { return None }
  Some(self.entries[index].id)
}

///|
fn IdentifierTable::insert(
  self : IdentifierTable,
  spelling : StringView,
  id : Int,
) -> Unit {
  if (self.entries.length() + 1) * 4 >= self.slots.length() * 3 {
    self.grow()
  }
  let hash = identifier_hash(spelling)
  let slot = self.find_slot(spelling, hash)
  let entry = { spelling, hash, id, }
  let index = self.slots[slot]
  guard index < 0 else {
    self.entries[index] = entry
    return
  }
  self.slots[slot] = self.entries.length()
  self.entries.push(entry)
}

///|
fn IdentifierTable::grow(self : IdentifierTable) -> Unit {
  let slots = Array::make(self.slots.length() * 2, -1)
  let mask = slots.length() - 1
  for index, entry in self.entries {
    for slot = entry.hash & mask {
      guard slots[slot] < 0 else { continue (slot + 1) & mask }
      slots[slot] = index
      break
    }
  }
  self.slots = slots
}
