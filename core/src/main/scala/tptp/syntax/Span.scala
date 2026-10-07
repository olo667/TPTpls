package tptp.syntax

/** Half-open range of code-point offsets into a document. */
final case class Span(start: Int, end: Int) {
  require(0 <= start && start <= end, s"invalid span $start..$end")
  def length: Int = end - start
  def contains(offset: Int): Boolean = start <= offset && offset < end
  /** Like `contains`, but also true at the end offset (a cursor right after the span). */
  def touches(offset: Int): Boolean = start <= offset && offset <= end
  def cover(other: Span): Span = Span(math.min(start, other.start), math.max(end, other.end))
}

object Span {
  def empty(at: Int): Span = Span(at, at)
}
