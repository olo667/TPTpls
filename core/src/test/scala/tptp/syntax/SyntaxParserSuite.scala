package tptp.syntax

class SyntaxParserSuite extends munit.FunSuite {
  private def errorNodes(c: Cst): Vector[ErrorNode] = c match {
    case n: Node      => n.children.flatMap(errorNodes)
    case e: ErrorNode => Vector(e)
    case _: Token     => Vector.empty
  }
  private def recordHasErrors(c: Cst): Boolean = c match {
    case n: Node => n.hasErrors
    case _       => true
  }

  test("valid records parse without errors") {
    val parsed = SyntaxParser.parse("fof(a, axiom, p).\nfof(b, conjecture, ~ q(X)).\n")
    assertEquals(parsed.errors, Vector.empty)
    assertEquals(parsed.cst.records.size, 2)
    parsed.cst.records.foreach {
      case n: Node => assertEquals(n.kind.name, "tptp_input"); assert(!n.hasErrors)
      case other   => fail(s"unexpected $other")
    }
  }

  test("empty and comment-only input") {
    assertEquals(SyntaxParser.parse("").cst.records, Vector.empty)
    assertEquals(SyntaxParser.parse("% only a comment\n").errors, Vector.empty)
  }

  test("a missing terminator does not affect the next record") {
    val text = "fof(a,axiom,p(X)\nfof(b,axiom,q)."
    val parsed = SyntaxParser.parse(text)
    val bStart = text.indexOf("fof(b")
    assert(parsed.errors.nonEmpty)
    assert(parsed.errors.forall(_.span.end <= bStart), parsed.errors)
    assert(recordHasErrors(parsed.cst.records(0)))
    assert(!recordHasErrors(parsed.cst.records(1)))
  }

  test("a broken formula does not affect the next record") {
    val parsed = SyntaxParser.parse("fof(c, axiom, (r & ).\nfof(d,axiom,s).")
    assert(parsed.errors.nonEmpty)
    assert(!recordHasErrors(parsed.cst.records(1)))
  }

  test("a missing record terminator becomes a zero-width error node where it is missing") {
    // ANTLR inserts single missing tokens only where its prediction is already committed; a missing
    // terminator at the end of a record is the typical case (see the Task 5 ruling in the ledger).
    val parsed = SyntaxParser.parse("fof(a,axiom,p(X)")
    val missing = errorNodes(parsed.cst.records(0)).collect {
      case e @ ErrorNode(ErrorCause.MissingToken(_), _, _, _) => e
    }
    assertEquals(missing.map(_.cause), Vector(ErrorCause.MissingToken(Set(TokenKind.literal(").")))))
    assertEquals(missing.head.span, Span(16, 16))
    assertEquals(missing.head.message, "missing ').'")
    assertEquals(parsed.errors.map(_.span), Vector(Span(16, 16)))
  }

  test("an unexpected token is kept in an error node with its text") {
    val parsed = SyntaxParser.parse("fof(a,axiom,p q).")
    val skipped = errorNodes(parsed.cst.records(0)).flatMap(_.skipped).map(_.text)
    assert(skipped.contains("q"), parsed.cst)
    assertEquals(parsed.errors.size, 1)
  }

  test("junk before the first record is one error node") {
    val parsed = SyntaxParser.parse("hello fof(a,axiom,p).")
    parsed.cst.records(0) match {
      case e: ErrorNode =>
        assertEquals(e.cause, ErrorCause.UnexpectedToken(TokenKind.symbolic("Lower_word")))
        assertEquals(e.span, Span(0, 5))
      case other => fail(s"expected an error node, got $other")
    }
    assertEquals(parsed.errors.size, 1)
    assert(!recordHasErrors(parsed.cst.records(1)))
  }

  test("trailing tokens after a complete record are reported once") {
    val text = "fof(a,axiom,p). ) )\nfof(b,axiom,q)."
    val parsed = SyntaxParser.parse(text)
    assertEquals(parsed.errors.size, 1)
    assertEquals(parsed.errors.head.span.start, text.indexOf(") )"))
    assert(!recordHasErrors(parsed.cst.records(1)))
  }

  test("comments are kept on the hidden channel and do not affect records") {
    val text = "% header\nfof(a, axiom, p). /* block */\n%$ defined\nfof(b, axiom, q)."
    val parsed = SyntaxParser.parse(text)
    assertEquals(parsed.errors, Vector.empty)
    assertEquals(parsed.cst.records.size, 2)
    assertEquals(parsed.cst.comments.map(_.text), Vector("% header", "/* block */", "%$ defined"))
    assertEquals(parsed.cst.comments.head.span, Span(0, 8))
  }

}
