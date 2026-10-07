package tptp.mutation

import tptp.syntax.*

class MutatorSuite extends munit.FunSuite {
  private val text = "fof(a,axiom,(p & q)).\nfof(b,axiom,p(X,c) != f(Y)).\n"

  private def mutants(kind: MutationKind, seed: Long = 1, t: String = text) =
    Mutator(seed).mutants(t, SyntaxParser.parse(t), kind, count = 20)

  test("mutants are deterministic for a seed and differ from the original") {
    MutationKind.values.foreach { kind =>
      val a = mutants(kind)
      assert(a.nonEmpty, kind)
      assertEquals(a.map(_.text), mutants(kind).map(_.text), kind)
      assert(a.forall(_.text != text), kind)
      assert(a.forall(m => m.expected.from <= m.expected.to), kind)
    }
  }

  test("deleting an element removes one whole term, formula or variable") {
    val texts = mutants(MutationKind.DeleteElement, t = "fof(a,axiom,(p & q)).").map(_.text).toSet
    assert(texts.subsetOf(Set("fof(a,axiom,( & q)).", "fof(a,axiom,(p & )).", "fof(a,axiom,()).", "fof(a,axiom,).")), texts)
    // roles, names and annotations are not elements: deleting them is covered by other mutation kinds
    assert(texts.contains("fof(a,axiom,(p & ))."), texts)
    val m = mutants(MutationKind.DeleteElement, t = "fof(a,axiom,(p & q)).").find(_.text == "fof(a,axiom,(p & )).").get
    assertEquals((m.expected.from, m.expected.to), (16, 17)) // from the end of "&" to where the next token starts
  }

  test("deleting punctuation removes one ), ] or ).") {
    val texts = mutants(MutationKind.DeletePunctuation, t = "fof(a,axiom,p(X)).").map(_.text).toSet
    assertEquals(texts, Set("fof(a,axiom,p(X).", "fof(a,axiom,p(X)"))
  }

  test("typos change exactly one character") {
    mutants(MutationKind.Typo).foreach { m =>
      assertEquals(m.text.length, text.length)
      assertEquals(m.text.zip(text).count((a, b) => a != b), 1, m.description)
    }
  }

  test("a keyword typo may put errors into the previous record") {
    val keywordTypos = Mutator(3).mutants(text, SyntaxParser.parse(text), MutationKind.Typo, count = 200)
      .filter(m => m.text.startsWith("fof(a") && !m.text.contains("\nfof("))
    assert(keywordTypos.nonEmpty)
    assert(keywordTypos.forall(m => m.boundaryRecord == m.recordIndex - 1), keywordTypos.map(_.description))
  }

  test("truncation keeps a prefix; stray insertion adds one §; comma duplication adds one comma") {
    mutants(MutationKind.Truncate).foreach(m => assert(text.startsWith(m.text), m.description))
    mutants(MutationKind.InsertStray).foreach(m => assertEquals(m.text.count(_ == '§'), 1))
    mutants(MutationKind.DuplicateComma).foreach(m => assertEquals(m.text.count(_ == ','), text.count(_ == ',') + 1))
  }

  test("the checker accepts a correct parse and reports precision") {
    val m = mutants(MutationKind.DeleteElement, t = "fof(a,axiom,(p & q)).").find(_.text == "fof(a,axiom,(p & )).").get
    val result = Check.run(SyntaxParser.parse("fof(a,axiom,(p & q))."), m)
    assertEquals(result.violations, Vector.empty)
    assertEquals(result.precision, Precision.Exact)
  }
}
