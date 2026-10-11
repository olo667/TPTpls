package tptp.syntax

import tptp.syntax.tree.*

class SyntaxParserSuite extends munit.FunSuite {
  test("valid records parse without errors") {
    val parsed = SyntaxParser.parse("fof(a, axiom, p).\nfof(b, conjecture, ~ q(X)).\n")
    assertEquals(parsed.allErrors, Vector.empty)
    assertEquals(parsed.records.map(_.getClass.getSimpleName), Vector("FofRecord", "FofRecord"))
  }

  test("empty and comment-only input") {
    assertEquals(SyntaxParser.parse("").records, Vector.empty)
    assertEquals(SyntaxParser.parse("% only a comment\n").allErrors, Vector.empty)
  }

  test("a missing terminator does not affect the next record") {
    val text = "fof(a,axiom,p(X)\nfof(b,axiom,q)."
    val parsed = SyntaxParser.parse(text)
    val bStart = text.indexOf("fof(b")
    assert(parsed.allErrors.nonEmpty)
    assert(parsed.allErrors.forall(_.span.end <= bStart), parsed.allErrors)
    assertEquals(parsed.records(1).errors, Vector.empty)
  }

  test("a broken formula does not affect the next record") {
    val parsed = SyntaxParser.parse("fof(c, axiom, (r & ).\nfof(d,axiom,s).")
    assert(parsed.allErrors.nonEmpty)
    assertEquals(parsed.records(1).errors, Vector.empty)
  }

  test("a missing record terminator is reported where it is missing") {
    val parsed = SyntaxParser.parse("fof(a,axiom,p(X)")
    assertEquals(parsed.allErrors.map(e => (e.message, e.span)), Vector(("missing ')'", Span(16, 16)), ("missing '.'", Span(16, 16))))
  }

  test("an unexpected token is reported with its text") {
    val parsed = SyntaxParser.parse("fof(a,axiom,p q).")
    assertEquals(parsed.allErrors.map(_.message), Vector("unexpected 'q'"))
  }

  test("trailing tokens after a complete record are reported once") {
    val text = "fof(a,axiom,p). ) )\nfof(b,axiom,q)."
    val parsed = SyntaxParser.parse(text)
    assertEquals(parsed.allErrors.size, 1)
    assertEquals(parsed.allErrors.head.span.start, text.indexOf(") )"))
    assertEquals(parsed.records(1).errors, Vector.empty)
  }

  test("comments do not affect records") {
    val text = "% header\nfof(a, axiom, p). /* block */\n%$ defined\nfof(b, axiom, q)."
    val parsed = SyntaxParser.parse(text)
    assertEquals(parsed.allErrors, Vector.empty)
    assertEquals(parsed.records.size, 2)
  }

  test("a lexer error skips one character, so an unterminated quote does not swallow the rest of the line") {
    val parsed = SyntaxParser.parse("fof('abc, axiom, p).")
    assertEquals(parsed.allErrors.map(e => (e.cause, e.span)), Vector((ErrorCause.LexerError, Span(4, 5))))
    assertEquals(parsed.allErrors.map(_.message), Vector("unexpected character '''"))
    assertEquals(parsed.records.size, 1)
  }
}
