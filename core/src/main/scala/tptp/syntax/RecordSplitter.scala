package tptp.syntax

import org.antlr.v4.runtime.Token as AntlrToken

enum Segment {
  case Record(tokens: Vector[AntlrToken])
  /** Tokens before the first record keyword. */
  case Junk(tokens: Vector[AntlrToken])
}

/** Cuts a token stream before every record keyword, so an error can never spill into the next record. */
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
    for (t <- tokens) {
      if (recordStarts(TokenKind(t.getType))) {
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
}
