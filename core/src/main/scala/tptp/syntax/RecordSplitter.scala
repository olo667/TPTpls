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

  def split(tokens: Vector[AntlrToken]): Vector[Segment] = {
    val out = Vector.newBuilder[Segment]
    var current = Vector.newBuilder[AntlrToken]
    var currentIsRecord = false
    var nonEmpty = false
    def flush(): Unit =
      if (nonEmpty) {
        val ts = current.result()
        out += (if (currentIsRecord) Segment.Record(ts) else Segment.Junk(ts))
      }
    for ((t, i) <- tokens.zipWithIndex) {
      if (startsRecord(tokens, i)) {
        flush()
        current = Vector.newBuilder[AntlrToken]
        currentIsRecord = true
        nonEmpty = false
      }
      current += t
      nonEmpty = true
    }
    flush()
    out.result()
  }

  private def startsRecord(tokens: Vector[AntlrToken], i: Int): Boolean =
    recordStarts(TokenKind(tokens(i).getType)) && (
      tokens.lift(i + 1).exists(next => TokenKind(next.getType) == Grammar.Tokens.openParen) ||
        tokens.lift(i - 1).exists(prev => TokenKind(prev.getType) == Grammar.Tokens.recordEnd)
    )
}
