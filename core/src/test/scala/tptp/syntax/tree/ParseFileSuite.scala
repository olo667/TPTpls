package tptp.syntax.tree

import tptp.syntax.{Element, ErrorCause, Span, SyntaxParser, TokenKind}

class ParseFileSuite extends munit.FunSuite {
  private def errorNodes(t: Tree): Vector[ErrorNode] = t match {
    case e: ErrorNode => Vector(e)
    case other        => other.children.toVector.flatMap(errorNodes)
  }
  private def causeAndSpan(e: ErrorNode): (ErrorCause, Span) = (e.cause, e.span)
  private def first(text: String): Record = SyntaxParser.parse(text).records.head

  test("a hole for a formula is an error node and an error at the gap") {
    val r = first("fof(a,axiom,(p & )).")
    assertEquals(errorNodes(r).map(causeAndSpan), Vector((ErrorCause.MissingElement(Element.Formula), Span(17, 17))))
    assertEquals(r.errors.map(e => (e.message, e.span)), Vector(("missing formula", Span(17, 17))))
  }

  test("a hole for an argument is an error node in the term position") {
    val r = first("fof(a,axiom,p(X,)).")
    assertEquals(errorNodes(r).map(causeAndSpan), Vector((ErrorCause.MissingElement(Element.Term), Span(16, 16))))
  }

  test("a hole in a name position is an error node there, and the record stays a FOF record") {
    first("fof(,axiom,p).") match {
      case r: FofRecord =>
        assertEquals(causeAndSpan(r.name.asInstanceOf[ErrorNode]), (ErrorCause.MissingElement(Element.Name), Span(4, 4)))
        assertEquals(r.errors.map(_.message), Vector("missing name"))
      case other => fail(s"unexpected $other")
    }
    first("fof(a,axiom,! [] : p).") match {
      case r: FofRecord =>
        assertEquals(r.errors.map(_.message), Vector("missing variable"))
        assertEquals(errorNodes(r).map(causeAndSpan), Vector((ErrorCause.MissingElement(Element.Variable), Span(15, 15))))
      case other => fail(s"unexpected $other")
    }
  }

  test("inserted punctuation and deleted tokens leave no node, only errors") {
    val missing = first("fof(a,axiom,p(X).")
    assertEquals(errorNodes(missing), Vector.empty)
    assertEquals(missing.errors.map(e => (e.message, e.span)), Vector(("missing ')'", Span(16, 16))))
    val extra = first("fof(a,axiom,p q).")
    assertEquals(errorNodes(extra), Vector.empty)
    assertEquals(extra.errors.map(e => (e.cause, e.span)),
      Vector((ErrorCause.UnexpectedToken(TokenKind.symbolic("Lower_word")), Span(14, 15))))
  }

  test("a collapsed bracket group is an error node spanning the group's contents") {
    val r = first("fof(a,axiom,p(f(@ @ @ @), b)).")
    val groups = errorNodes(r).map(causeAndSpan).collect { case (ErrorCause.Unparsable(_), s) => s }
    assertEquals(groups, Vector(Span(16, 23)))
    assert(TreePrinter.print(r, 0).contains("Name \"b\""))
    // also in a variable list: the whole group is one error node among the variables
    first("fof(a,axiom,! [X @ @ @ @ Y] : p(X)).") match {
      case FofRecord(_, _, _, Logical(q: QuantifiedFormula, _), _, _, _) =>
        assertEquals(q.vars.collect { case e: ErrorNode => causeAndSpan(e) }.map(_._2), Vector(Span(15, 26)))
      case other => fail(s"unexpected $other")
    }
  }

  test("default recovery lowers incomplete rules to error nodes") {
    val file = SyntaxParser.parse("fof(a,axiom,p(f(g(h(X\nfof(b,axiom,q).")
    assert(file.records(0).errors.nonEmpty)
    assert(file.records(0).isInstanceOf[FofRecord], file.records(0))
    assertEquals(file.records(1).errors, Vector.empty)
  }

  test("trailing tokens after a record are one error, inside the record's span") {
    val text = "fof(a,axiom,p). ) )\nfof(b,axiom,q)."
    val file = SyntaxParser.parse(text)
    assertEquals(file.records(0).errors.map(_.span.start), Vector(text.indexOf(") )")))
    assertEquals(file.records(0).span.end, text.indexOf("\n"))
    assertEquals(file.records(1).errors, Vector.empty)
  }

  test("junk before the first record is an error record") {
    SyntaxParser.parse("hello fof(a,axiom,p).").records match {
      case Vector(j: ErrorRecord, r: FofRecord) =>
        assertEquals(j.span, Span(0, 5))
        assertEquals(j.cause, ErrorCause.UnexpectedToken(TokenKind.symbolic("Lower_word")))
        assertEquals(j.errors.size, 1)
        assertEquals(r.errors, Vector.empty)
      case other => fail(s"unexpected $other")
    }
  }

  test("lexer errors belong to the file; an unterminated quote does not swallow the record") {
    val file = SyntaxParser.parse("fof('abc, axiom, p).")
    assertEquals(file.errors.map(e => (e.cause, e.span)), Vector((ErrorCause.LexerError, Span(4, 5))))
    assertEquals(file.records.map(_.errors), Vector(Vector.empty))
  }

  test("holes outside FOF formulas are named by their rule") {
    def messages(text: String) = SyntaxParser.parse(text).allErrors.map(_.message)
    assertEquals(messages("cnf(a,axiom,p | )."), Vector("missing cnf literal"))
    assertEquals(messages("thf(a, type, c: )."), Vector("missing thf top level type"))
    assertEquals(messages("fof(a,axiom,p, )."), Vector("missing source"))
  }

  test("errors of unlowered formulas are still reported") {
    first("thf(a, type, c: ).") match {
      case r: OtherRecord => assert(r.errors.nonEmpty)
      case other          => fail(s"unexpected $other")
    }
  }
}
