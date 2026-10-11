package tptp.syntax

import org.antlr.v4.runtime.Token as AntlrToken

enum Segment {
  case Record(tokens: Vector[AntlrToken])
  /** Tokens before the first record keyword. */
  case Junk(tokens: Vector[AntlrToken])
}

/** Cuts a token stream before every record keyword that is followed by `(` or follows the `.` ending a record,
  * so an error can never spill into the next record. Other keywords are words inside a record, e.g. the
  * constant `include` in `p(include)`. */
object RecordSplitter {
  val recordStarts: Set[TokenKind] = Grammar.Tokens.recordKeywords

  def split(tokens: Iterator[AntlrToken]): Iterator[Segment] = new Iterator[Segment] {
    private val ahead = scala.collection.mutable.Queue.empty[AntlrToken]
    private var previous: Option[AntlrToken] = None
    private def peek(n: Int): Option[AntlrToken] = {
      while (ahead.size <= n && tokens.hasNext) ahead += tokens.next()
      ahead.lift(n)
    }
    private def startsHere: Boolean = peek(0).exists(t => startsRecord(previous, t, peek(1)))

    def hasNext: Boolean = peek(0).isDefined

    def next(): Segment = {
      if (!hasNext) throw new NoSuchElementException("no more segments")
      val isRecord = startsHere
      val out = Vector.newBuilder[AntlrToken]
      var first = true
      while (peek(0).isDefined && (first || !startsHere)) {
        val t = ahead.dequeue()
        out += t
        previous = Some(t)
        first = false
      }
      if (isRecord) Segment.Record(out.result()) else Segment.Junk(out.result())
    }
  }

  private def startsRecord(previous: Option[AntlrToken], t: AntlrToken, next: Option[AntlrToken]): Boolean =
    recordStarts(TokenKind(t.getType)) && (
      next.exists(n => TokenKind(n.getType) == Grammar.Tokens.openParen) ||
        previous.exists(p => TokenKind(p.getType) == Grammar.Tokens.recordEnd)
    )
}
