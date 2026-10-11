package tptp.syntax.tree

import org.antlr.v4.runtime.{CommonTokenStream, ListTokenSource}
import scala.jdk.CollectionConverters.*
import tptp.syntax.{LexerDriver, Span}
import tptp.syntax.generated.TPTPParser

class LoweringSuite extends munit.FunSuite {
  /** Lowers one valid record, parsed directly (no repair). */
  private def lower(text: String): Record = {
    val tokens = LexerDriver.lex(text).tokens
    val parser = new TPTPParser(new CommonTokenStream(new ListTokenSource(tokens.asJava)))
    Lowering.record(parser.tptp_input(), Vector.empty, None)
  }
  private def formula(text: String): Formula = lower(s"fof(a, axiom, $text).") match {
    case FofRecord(_, _, _, Logical(f, _), _, _, _) => f
    case other                                      => fail(s"unexpected $other")
  }

  test("a FOF record keeps its name, role, formula and annotation span") {
    val text = "fof('a b', axiom-x, p(X), file('f.ax', n))."
    assertNoDiff(TreePrinter.print(lower(text), 0),
      """FofRecord 0..43 annotations 24..41
        |  Name "'a b'" 4..9
        |  Role 11..18 detail 17..18
        |    Name "axiom" 11..16
        |  Logical 20..24
        |    AtomicFormula Plain 20..24
        |      Name "p" 20..21
        |      Variable 22..23
        |        Name "X" 22..23
        |""".stripMargin)
  }

  test("associative lists nest to the left") {
    formula("p & q & r") match {
      case BinaryFormula(Op(BinaryConnective.And, _), BinaryFormula(_, p: AtomicFormula, q: AtomicFormula, inner, _),
                         r: AtomicFormula, outer, _) =>
        assertEquals((p.f.text, q.f.text, r.f.text), ("p", "q", "r"))
        assertEquals((inner, outer), (Span(14, 19), Span(14, 23)))
      case other => fail(s"unexpected $other")
    }
  }

  test("parentheses widen outer, not span") {
    formula("((p => q))") match {
      case b: BinaryFormula =>
        assertEquals(b.c.kind, BinaryConnective.Impl)
        assertEquals(b.span, Span(16, 22))
        assertEquals(b.outer, Span(14, 24))
        assert(b.isUnitary)
      case other => fail(s"unexpected $other")
    }
  }

  test("quantifiers, negation, equality, inequality and defined terms") {
    formula("! [X, Y] : ~ (X = Y | f(X) != \"o\" | $less(X, 1/2))") match {
      case QuantifiedFormula(Op(Quantifier.ForAll, _), vars, UnaryFormula(_, body, _, _), _, _) =>
        assertEquals(vars.flatMap(_.valid).map(_.text), Vector("X", "Y"))
        body match {
          case BinaryFormula(_, BinaryFormula(_, Equality(_, _, _, _), Inequality(_, DistinctObject("\"o\"", _), _, _), _, _),
                             a: AtomicFormula, _, _) =>
            assertEquals(a.kind, AtomKind.Defined)
            assertEquals(a.args.collect { case NumberTerm(t, _) => t }, Vector("1/2"))
          case other => fail(s"unexpected $other")
        }
      case other => fail(s"unexpected $other")
    }
  }

  test("a sequent lowers both tuples") {
    lower("fof(s, axiom, [p, q] --> [r]).") match {
      case FofRecord(_, _, _, Sequent(l, r, _), _, _, _) => assertEquals((l.size, r.size), (2, 1))
      case other                                         => fail(s"unexpected $other")
    }
  }

  test("includes keep file name, selection and space name") {
    lower("include('Axioms/a\\'b.ax', [n1, 'n2', 3], sp).") match {
      case i: Include =>
        assertEquals(i.fileName.valid.map(_.key), Some("Axioms/a'b.ax"))
        assertEquals(i.selection.map(_.flatMap(_.valid).map(_.key)), Some(Vector("n1", "n2", "3")))
        assertEquals(i.space.flatMap(_.valid).map(_.text), Some("sp"))
      case other => fail(s"unexpected $other")
    }
    lower("include('x.ax', *).") match {
      case i: Include => assertEquals(i.selection, None)
      case other      => fail(s"unexpected $other")
    }
  }

  test("records of other languages keep keyword, name and role only") {
    lower("thf(c_type, type, c: $i).") match {
      case r: OtherRecord =>
        assertEquals(r.language, Language.Thf)
        assertEquals(r.keyword, Span(0, 3))
        assertEquals((r.name.valid.map(_.text), r.role.word.valid.map(_.text)), (Some("c_type"), Some("type")))
      case other => fail(s"unexpected $other")
    }
  }

  test("deeply nested formulas lower") {
    val depth = 3000
    assert(formula("(" * depth + "p" + ")" * depth).isInstanceOf[AtomicFormula])
  }
}
