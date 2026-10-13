///|
/// A list under construction owns the buffer suffix starting at `start`.
/// Nested lists finish first; releasing a list restores its parent's suffix.
#valtype
priv struct PendingEdges {
  buffer : Array[Int32]
  start : Int32
}

///|
fn Parser::begin_edges(self : Parser) -> PendingEdges {
  { buffer: self.pending_edges, start: self.pending_edges.length(), }
}

///|
fn PendingEdges::push(self : PendingEdges, item : Int32) -> Unit {
  self.buffer.push(item)
}

///|
fn PendingEdges::length(self : PendingEdges) -> Int32 {
  self.buffer.length() - self.start
}

///|
#alias("_[_]")
fn PendingEdges::at(self : PendingEdges, index : Int32) -> Int32 {
  self.buffer[self.start + index]
}

///|
fn PendingEdges::view(self : PendingEdges) -> ArrayView[Int32] {
  self.buffer[self.start:]
}

///|
fn PendingEdges::release(self : PendingEdges) -> Unit {
  self.buffer.truncate(self.start)
}
