package tptp.syntax

import tptp.syntax.tree.*

class RecordRepairSuite extends munit.FunSuite {
  private def errorNodes(t: Tree): Vector[(ErrorCause, Span)] = t match {
    case e: ErrorNode => Vector((e.cause, e.span))
    case other        => other.children.toVector.flatMap(errorNodes)
  }
  private def names(t: Tree): Vector[String] = t match {
    case n: Name => Vector(n.text)
    case other   => other.children.toVector.flatMap(names)
  }
  private def first(text: String): (TptpFile, Record) = {
    val parsed = SyntaxParser.parse(text)
    (parsed, parsed.records.head)
  }

  test("a missing operand becomes a formula hole exactly at the gap") {
    val (parsed, record) = first("fof(a,axiom,(p & )).")
    assertEquals(errorNodes(record), Vector((ErrorCause.MissingElement(Element.Formula), Span(17, 17))))
    assertEquals(parsed.allErrors.map(e => (e.message, e.span)), Vector(("missing formula", Span(17, 17))))
  }

  test("a missing role is reported as a role, not a formula") {
    val (parsed, _) = first("fof(a,,p).")
    assertEquals(parsed.allErrors.map(_.message), Vector("missing role"))
  }

  test("a missing argument becomes a term hole") {
    val (parsed, record) = first("fof(a,axiom,p(X,)).")
    assertEquals(errorNodes(record), Vector((ErrorCause.MissingElement(Element.Term), Span(16, 16))))
    assertEquals(parsed.allErrors.size, 1)
  }

  test("an empty variable list gets a variable hole between the brackets") {
    val (parsed, _) = first("fof(a,axiom,! [] : p).")
    assertEquals(parsed.allErrors.map(e => (e.message, e.span)), Vector(("missing variable", Span(15, 15))))
  }

  test("a missing parenthesis is inserted and the formula keeps its structure") {
    val (parsed, record) = first("fof(a,axiom,p(X).")
    // the `)` present closes `p(`; the one missing is the record's, before `.`
    assertEquals(parsed.allErrors.map(e => (e.cause, e.span)), Vector((ErrorCause.MissingToken(Set(TokenKind.literal(")"))), Span(16, 16))))
    assertEquals(parsed.allErrors.map(_.message), Vector("missing ')'"))
    assertEquals(names(record), Vector("a", "axiom", "p", "X"))
  }

  test("two omissions give two errors at the gap") {
    val (parsed, _) = first("fof(a,axiom,(p & ).")
    assertEquals(parsed.allErrors.map(e => (e.cause, e.span)), Vector(
      (ErrorCause.MissingElement(Element.Formula), Span(17, 17)),
      (ErrorCause.MissingToken(Set(TokenKind.literal(")"))), Span(18, 18)),
    ))
  }

  test("an unfinished record at the end of the file is completed") {
    val (parsed, _) = first("fof(a,axiom,p & ")
    assertEquals(parsed.allErrors.map(_.cause).toSet,
      Set(ErrorCause.MissingElement(Element.Formula), ErrorCause.MissingToken(Set(TokenKind.literal(")"))),
        ErrorCause.MissingToken(Set(TokenKind.literal(".")))))
    assert(parsed.allErrors.forall(_.span == Span(15, 15)), parsed.allErrors)
  }

  test("an extra token is deleted and reported inside the record") {
    val (parsed, record) = first("fof(a,axiom,p q).")
    assertEquals(record.errors.map(e => (e.cause, e.span)),
      Vector((ErrorCause.UnexpectedToken(TokenKind.symbolic("Lower_word")), Span(14, 15))))
    assertEquals(parsed.allErrors.map(_.message), Vector("unexpected 'q'"))
  }

  test("records needing more edits than allowed fall back to default recovery, isolated") {
    val parsed = SyntaxParser.parse("fof(a,axiom,p(f(g(h(X\nfof(b,axiom,q).")
    assert(parsed.allErrors.nonEmpty)
    assertEquals(parsed.records(1).errors, Vector.empty)
  }

  test("the repair search is bounded on large broken records") {
    val text = "fof(a,axiom," + ("@ " * 300) + ")."
    val t0 = System.nanoTime
    val parsed = SyntaxParser.parse(text)
    val ms = (System.nanoTime - t0) / 1000000
    assert(parsed.allErrors.nonEmpty)
    assert(ms < 3000, s"took $ms ms")
  }

  test("repair work is limited per file; later broken records use default recovery") {
    val unrepairable = "fof(u,axiom,p(f(g(h(X\n" * 40
    val text = "fof(a,axiom,(p & )).\n" + unrepairable + "fof(z,axiom,(p & )).\n"
    val t0 = System.nanoTime
    val parsed = SyntaxParser.parse(text)
    val ms = (System.nanoTime - t0) / 1000000
    val firstCauses = parsed.records.head.errors.map(_.cause)
    val lastCauses = parsed.records.last.errors.map(_.cause)
    assertEquals(firstCauses, Vector(ErrorCause.MissingElement(Element.Formula)))
    assert(lastCauses.nonEmpty && !lastCauses.contains(ErrorCause.MissingElement(Element.Formula)), lastCauses)
    assert(ms < 5000, s"took $ms ms")
  }

  /** (spans of the contained groups, other error causes, names in the tree, diagnostic spans) */
  private def contained(text: String) = {
    val (parsed, record) = first(text)
    (errorNodes(record).collect { case (ErrorCause.Unparsable(_), span) => span },
     record.errors.map(_.cause).filter { case ErrorCause.Unparsable(_) => false; case _ => true },
     names(record), parsed.allErrors.map(_.span))
  }

  test("an unrepairable part inside brackets is contained to that bracket group") {
    val (groups, others, texts, diagnostics) = contained("fof(a,axiom,(p & (q @ @ @ @ r)) | s).")
    assertEquals(groups, Vector(Span(18, 29)))
    assertEquals(others, Vector.empty)
    assert(Set("p", "s").subsetOf(texts.toSet), texts)
    assertEquals(diagnostics, Vector(Span(20, 21)), "the diagnostic points at the first '@', where parsing failed")
  }

  test("an unrepairable argument list is contained to its brackets") {
    val (groups, _, texts, _) = contained("fof(a,axiom,p(f(@ @ @ @), b)).")
    assertEquals(groups, Vector(Span(16, 23)))
    assert(texts.contains("b"), texts)
  }

  test("an unrepairable variable list is contained to its brackets") {
    val (groups, _, texts, _) = contained("fof(a,axiom,! [X @ @ @ @ Y] : p(X)).")
    assertEquals(groups, Vector(Span(15, 26)))
    assert(texts.contains("p"), texts)
  }

  test("two broken groups give two contained errors") {
    val (groups, others, _, diagnostics) = contained("fof(a,axiom,(p @ @ @ @ q) & (r @ @ @ @ s)).")
    assertEquals(groups, Vector(Span(13, 24), Span(29, 40)))
    assertEquals(others, Vector.empty)
    assertEquals(diagnostics.size, 2)
  }

  test("a contained group combines with a cheap repair elsewhere") {
    val (groups, others, texts, _) = contained("fof(a,axiom, p => (q $ $ $ $ r).")
    assertEquals(groups, Vector(Span(19, 30)))
    assertEquals(others, Vector(ErrorCause.MissingToken(Set(TokenKind.literal(")")))))
    assert(texts.contains("p"), texts)
  }

  test("without brackets around the error, default recovery is used as before") {
    val (groups, others, _, _) = contained("fof(a,axiom,p @ @ @ @ q).")
    assertEquals(groups, Vector.empty)
    assert(others.nonEmpty)
  }
}
