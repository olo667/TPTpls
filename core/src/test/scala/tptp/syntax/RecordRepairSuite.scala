package tptp.syntax

class RecordRepairSuite extends munit.FunSuite {
  private def errorNodes(c: Cst): Vector[ErrorNode] = c match {
    case n: Node      => n.children.flatMap(errorNodes)
    case e: ErrorNode => Vector(e)
    case _: Token     => Vector.empty
  }
  private def tokens(c: Cst): Vector[Token] = c match {
    case n: Node      => n.children.flatMap(tokens)
    case _: ErrorNode => Vector.empty
    case t: Token     => Vector(t)
  }
  private def first(text: String): (ParsedFile, Cst) = {
    val parsed = SyntaxParser.parse(text)
    (parsed, parsed.cst.records.head)
  }

  test("a missing operand becomes a formula hole exactly at the gap") {
    val (parsed, record) = first("fof(a,axiom,(p & )).")
    assertEquals(errorNodes(record).map(e => (e.cause, e.span)), Vector((ErrorCause.MissingElement("formula"), Span(17, 17))))
    assertEquals(parsed.errors.map(e => (e.message, e.span)), Vector(("missing formula", Span(17, 17))))
    assertEquals(tokens(record).map(_.text), Vector("fof(", "a", ",", "axiom", ",", "(", "p", "&", ")", ")."))
  }

  test("a missing role is reported as a role, not a formula") {
    val (parsed, _) = first("fof(a,,p).")
    assertEquals(parsed.errors.map(_.message), Vector("missing role"))
  }

  test("a missing argument becomes a term hole") {
    val (parsed, record) = first("fof(a,axiom,p(X,)).")
    assertEquals(errorNodes(record).map(e => (e.cause, e.span)), Vector((ErrorCause.MissingElement("term"), Span(16, 16))))
    assertEquals(parsed.errors.size, 1)
  }

  test("an empty variable list is split and gets a variable hole between the brackets") {
    val (parsed, record) = first("fof(a,axiom,! [] : p).")
    assertEquals(errorNodes(record).map(e => (e.cause, e.span)), Vector((ErrorCause.MissingElement("variable"), Span(15, 15))))
    assertEquals(tokens(record).filter(t => t.text == "[" || t.text == "]").map(t => (t.text, t.span)),
      Vector(("[", Span(14, 15)), ("]", Span(15, 16))))
    assertEquals(parsed.errors.map(_.message), Vector("missing variable"))
  }

  test("a missing parenthesis is inserted and the formula keeps its structure") {
    val (parsed, record) = first("fof(a,axiom,p(X).")
    assertEquals(errorNodes(record).map(e => (e.cause, e.span)),
      Vector((ErrorCause.MissingToken(Set(TokenKind.literal(")"))), Span(15, 15))))
    assertEquals(tokens(record).map(_.text), Vector("fof(", "a", ",", "axiom", ",", "p", "(", "X", ")."))
    assertEquals(parsed.errors.map(_.message), Vector("missing ')'"))
  }

  test("two omissions give two errors at the gap") {
    val (parsed, record) = first("fof(a,axiom,(p & ).")
    assertEquals(errorNodes(record).map(e => (e.cause, e.span)), Vector(
      (ErrorCause.MissingElement("formula"), Span(17, 17)),
      (ErrorCause.MissingToken(Set(TokenKind.literal(")"))), Span(17, 17)),
    ))
    assertEquals(parsed.errors.size, 2)
  }

  test("an unfinished record at the end of the file is completed") {
    val (parsed, record) = first("fof(a,axiom,p & ")
    assertEquals(errorNodes(record).map(_.cause).toSet,
      Set(ErrorCause.MissingElement("formula"), ErrorCause.MissingToken(Set(TokenKind.literal(").")))))
    assert(parsed.errors.forall(_.span == Span(15, 15)), parsed.errors)
  }

  test("an extra token is deleted and kept as an error node inside the record") {
    val (parsed, record) = first("fof(a,axiom,p q).")
    val errs = errorNodes(record)
    assertEquals(errs.map(e => (e.cause, e.span, e.skipped.map(_.text))),
      Vector((ErrorCause.UnexpectedToken(TokenKind.symbolic("Lower_word")), Span(14, 15), Vector("q"))))
    assertEquals(parsed.errors.map(_.message), Vector("unexpected 'q'"))
  }

  test("records needing more edits than allowed fall back to default recovery, isolated") {
    val parsed = SyntaxParser.parse("fof(a,axiom,p(f(g(h(X\nfof(b,axiom,q).")
    assert(parsed.errors.nonEmpty)
    parsed.cst.records(1) match {
      case n: Node => assert(!n.hasErrors)
      case other   => fail(s"unexpected $other")
    }
  }

  test("the repair search is bounded on large broken records") {
    val text = "fof(a,axiom," + ("§ " * 300) + ")."
    val t0 = System.nanoTime
    val parsed = SyntaxParser.parse(text)
    val ms = (System.nanoTime - t0) / 1000000
    assert(parsed.errors.nonEmpty)
    assert(ms < 3000, s"took $ms ms")
  }

  test("repair work is limited per file; later broken records use default recovery") {
    val unrepairable = "fof(u,axiom,p(f(g(h(X\n" * 40
    val text = "fof(a,axiom,(p & )).\n" + unrepairable + "fof(z,axiom,(p & )).\n"
    val t0 = System.nanoTime
    val parsed = SyntaxParser.parse(text)
    val ms = (System.nanoTime - t0) / 1000000
    val firstCauses = errorNodes(parsed.cst.records.head).map(_.cause)
    val lastCauses = errorNodes(parsed.cst.records.last).map(_.cause)
    assertEquals(firstCauses, Vector(ErrorCause.MissingElement("formula")))
    assert(lastCauses.nonEmpty && !lastCauses.contains(ErrorCause.MissingElement("formula")), lastCauses)
    assert(ms < 5000, s"took $ms ms")
  }

  test("the split table comes from the grammar") {
    val splits = RecordRepair.splits.map { case (whole, (a, b)) => (whole.name, a.name, b.name) }.toSet
    assert(splits.contains(("'[]'", "'['", "']'")), splits)
    assert(splits.contains(("').'", "')'", "'.'")), splits)
    assert(!splits.exists(_._1 == "'<=>'"), splits)
    assert(!splits.exists(_._1 == "'fof('"), splits)
  }
}
