package tptp.syntax

class ViewsSuite extends munit.FunSuite {
  private def records(text: String): Vector[Option[Record]] = SyntaxParser.parse(text).cst.records.map(Record.of)

  test("annotated formulas expose language, name and role") {
    val Vector(Some(Record.Formula(f)), Some(Record.Formula(t))) =
      records("fof('b', axiom, p).\nthf(c_type, type, c: $i).") : @unchecked
    assertEquals(f.language, "fof")
    assertEquals(f.name.map(Names.key), Some("b"))
    assertEquals(f.name.map(_.text), Some("'b'"))
    assertEquals(f.role.map(_.text), Some("axiom"))
    assertEquals(f.keyword.map(_.text), Some("fof("))
    assertEquals(t.language, "thf")
    assertEquals(t.role.map(_.text), Some("type"))
  }

  test("integer formula names") {
    val Vector(Some(Record.Formula(f))) = records("cnf(42, axiom, p).") : @unchecked
    assertEquals(f.name.map(Names.key), Some("42"))
  }

  test("includes expose an unescaped file name and the selection") {
    val Vector(Some(Record.Include(a)), Some(Record.Include(b)), Some(Record.Include(c))) =
      records("include('Axioms/a\\'b.ax').\ninclude('x.ax', [n1, 'n2', 3]).\ninclude('y.ax', *).") : @unchecked
    assertEquals(a.fileName, Some("Axioms/a'b.ax"))
    assertEquals(a.selection, None)
    assertEquals(b.selection.map(_.map(Names.key)), Some(Vector("n1", "n2", "3")))
    assertEquals(c.selection, None)
    assertEquals(b.fileNameToken.map(_.span), Some(Span(35, 41)))
  }

  test("broken records give partial views; junk gives none") {
    val parsed = SyntaxParser.parse("junk fof(X, axiom, p).")
    assertEquals(Record.of(parsed.cst.records(0)), None)
    Record.of(parsed.cst.records(1)) match {
      case Some(Record.Formula(f)) =>
        assertEquals(f.name, None)
        assertEquals(f.role.map(_.text), Some("axiom"))
      case other                   => fail(s"expected a formula view, got $other")
    }
  }

  test("Names.unquote handles escaped quotes and backslashes") {
    assertEquals(Names.unquote("'a\\'b\\\\c'"), "a'b\\c")
  }
}
