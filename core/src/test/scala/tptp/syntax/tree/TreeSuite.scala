package tptp.syntax.tree

import tptp.syntax.Span

class TreeSuite extends munit.FunSuite {
  test("a single-quoted name has the identity of its unquoted, unescaped content") {
    assertEquals(Name("'b'", Span(0, 3)).key, "b")
    assertEquals(Name("b", Span(0, 1)).key, "b")
    assertEquals(Name("'a\\'b\\\\c'", Span(0, 9)).key, "a'b\\c")
    assertEquals(Name("42", Span(0, 2)).key, "42")
  }

  test("a formula in parentheses is unitary whatever its class") {
    val p = AtomicFormula(Name("p", Span(1, 2)), Nil, Span(1, 2), Span(1, 2))
    val q = AtomicFormula(Name("q", Span(5, 6)), Nil, Span(5, 6), Span(5, 6))
    val and = BinaryFormula(Op(BinaryConnective.And, Span(3, 4)), p, q, Span(1, 6), Span(1, 6))
    assert(p.isUnitary)
    assert(!and.isUnitary)
    assert(and.withOuter(Span(0, 7)).isUnitary)
  }
}
