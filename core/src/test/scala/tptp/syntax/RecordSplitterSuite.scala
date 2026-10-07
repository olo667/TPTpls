package tptp.syntax

class RecordSplitterSuite extends munit.FunSuite {
  private def segments(text: String): Vector[(String, String)] =
    RecordSplitter.split(LexerDriver.lex(text).tokens).map {
      case Segment.Record(ts) => "record" -> ts.map(_.getText).mkString(" ")
      case Segment.Junk(ts)   => "junk" -> ts.map(_.getText).mkString(" ")
    }

  test("splits before every record keyword") {
    assertEquals(
      segments("fof(a,axiom,p).\ninclude('x.ax').\nthf(b,type,c: $i)."),
      Vector(
        "record" -> "fof( a , axiom , p ).",
        "record" -> "include( 'x.ax' ).",
        "record" -> "thf( b , type , c : $i ).",
      ),
    )
  }

  test("tokens before the first record are junk; tokens after a record stay in it") {
    assertEquals(
      segments("hello world fof(a,axiom,p). trailing fof(b,axiom,q)."),
      Vector(
        "junk" -> "hello world",
        "record" -> "fof( a , axiom , p ). trailing",
        "record" -> "fof( b , axiom , q ).",
      ),
    )
  }

  test("comments and whitespace produce no tokens; empty input has no segments") {
    assertEquals(segments("% comment\n/* block */\n  \n"), Vector.empty)
  }

  test("token offsets are code points") {
    val ts = LexerDriver.lex("fof(a,axiom,😀).").tokens
    val emoji = ts.find(_.getText == "😀").get
    assertEquals(AntlrTokens.toCst(emoji).span, Span(12, 13))
  }
}
