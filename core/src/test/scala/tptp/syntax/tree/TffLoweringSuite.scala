package tptp.syntax.tree

import org.antlr.v4.runtime.{CommonTokenStream, ListTokenSource}
import scala.jdk.CollectionConverters.*
import tptp.syntax.{ErrorCause, Element, LexerDriver, Span, SyntaxParser}
import tptp.syntax.generated.TPTPParser

class TffLoweringSuite extends munit.FunSuite {
  /** Lowers one valid record, parsed directly (no repair). */
  private def lower(text: String): Record = {
    val tokens = LexerDriver.lex(text).tokens
    val parser = new TPTPParser(new CommonTokenStream(new ListTokenSource(tokens.asJava)))
    Lowering.record(parser.tptp_input(), Vector.empty, None)
  }
  private def statement(text: String): Statement = lower(text) match {
    case r: TffRecord => r.statement
    case other        => fail(s"not a TFF record: $other")
  }
  private def formula(text: String): Formula = statement(s"tff(a, axiom, $text).") match {
    case Logical(f, _) => f
    case other         => fail(s"unexpected $other")
  }
  private def all(t: Tree): Vector[Tree] = t +: t.children.toVector.flatMap(all)
  private def unsupported(text: String): Vector[String] = all(lower(text)).collect { case u: Unsupported => u.rule }

  test("arguments are terms and atoms in formula positions are formulas") {
    formula("p(f(X), c, 1, \"o\")") match {
      case AtomicFormula(p, Seq(f: AtomicTerm, c: AtomicTerm, _: NumberTerm, _: DistinctObject), _, _) =>
        assertEquals((p.text, f.f.text, c.f.text), ("p", "f", "c"))
        assert(f.args.head.isInstanceOf[Variable])
      case other => fail(s"unexpected $other")
    }
  }

  test("equality and inequality sides are terms") {
    formula("f(X) = c & X != $sum(1, 2)") match {
      case BinaryFormula(_, Equality(_: AtomicTerm, _: AtomicTerm, _, _), Inequality(_: Variable, s: AtomicTerm, _, _), _, _) =>
        assertEquals(s.kind, AtomKind.Defined)
      case other => fail(s"unexpected $other")
    }
  }

  test("quantified variables keep their types") {
    formula("! [X: $int, Y, L: list(T)] : p(X, Y, L)") match {
      case QuantifiedFormula(_, Seq(x, y, l), _, _, _) =>
        x.ty match {
          case Some(TypeApp(f, Seq(), _)) => assertEquals(f.text, "$int")
          case other                      => fail(s"unexpected $other")
        }
        assertEquals(y.ty, None)
        l.ty match {
          case Some(TypeApp(f, Seq(TypeVariable(t, _)), _)) => assertEquals((f.text, t.text), ("list", "T"))
          case other                                         => fail(s"unexpected $other")
        }
      case other => fail(s"unexpected $other")
    }
  }

  test("type declarations: constants, mappings with flattened products, and polymorphic types") {
    statement("tff(t, type, f: (nat * list(nat) * $int) > $o).") match {
      case TypeDeclaration(f: Name, MappingType(args, TypeApp(o, Seq(), _), _), _) =>
        assertEquals((f.text, args.size, o.text), ("f", 3, "$o"))
      case other => fail(s"unexpected $other")
    }
    statement("tff(t, type, (nat: $tType)).") match {
      case TypeDeclaration(n: Name, TypeApp(k, Seq(), _), _) => assertEquals((n.text, k.text), ("nat", "$tType"))
      case other                                             => fail(s"unexpected $other")
    }
    statement("tff(t, type, cons: !>[T: $tType]: ((T * list(T)) > list(T))).") match {
      case TypeDeclaration(_, QuantifiedType(Seq(v), MappingType(Seq(_: TypeVariable, _: TypeApp), _: TypeApp, _), _), _) =>
        assertEquals(v.name.valid.map(_.text), Some("T"))
      case other => fail(s"unexpected $other")
    }
  }

  test("NX0 constructs are Unsupported, named by their rule") {
    assertEquals(unsupported("tff(a, axiom, {$box} @ (p))."), Vector("tff_defined_plain"))
    assertEquals(unsupported("tff(a, axiom, [.] p)."), Vector("tff_prefix_unary"))
    assertEquals(unsupported("tff(a, axiom, # [X] : p(X))."), Vector("tff_quantified_formula"))
  }

  test("a hole in a type position is a missing type") {
    SyntaxParser.parse("tff(t, type, f: (nat * ) > nat).").records.head match {
      case TffRecord(_, _, _, TypeDeclaration(_, MappingType(args, _, _), _), _, _, _) =>
        assertEquals(args.collect { case ErrorNode(ErrorCause.MissingElement(e), _, _, _) => e }, Vector(Element.Type))
      case other => fail(s"unexpected $other")
    }
  }

  test("a formula in a term position is a FormulaTerm; a variable in a formula position a VariableFormula") {
    formula("p(q & r, (s), X) | Y") match {
      // a parenthesised atom is a formula (BNF prose before `<tff_defined_infix>`)
      case BinaryFormula(_, AtomicFormula(_, Seq(FormulaTerm(_: BinaryFormula, _), FormulaTerm(s: AtomicFormula, _), _: Variable), _, _),
                         VariableFormula(y, _, _), _, _) =>
        assertEquals((s.f.text, y.text), ("s", "Y"))
      case other => fail(s"unexpected $other")
    }
  }

  test("tuples and tuple types") {
    formula("p([a, f(b)])") match {
      case AtomicFormula(_, Seq(Tuple(Seq(_: AtomicTerm, _: AtomicTerm), _)), _, _) => ()
      case other                                                                    => fail(s"unexpected $other")
    }
    statement("tff(t, type, f: [$i, nat] > $o).") match {
      case TypeDeclaration(_, MappingType(Seq(TupleType(Seq(_: TypeApp, _: TypeApp), _)), _, _), _) => ()
      case other => fail(s"unexpected $other")
    }
  }

  test("$let keeps its typings, definitions and body, as a formula and as a term") {
    formula("$let([f: $int > $int, c: $int], [f(X) := $sum(X, 1), c := 2], $less(f(c), 3))") match {
      case Let(Seq(t1, t2), Seq(LetDefinition(f: AtomicTerm, _: AtomicTerm, _), LetDefinition(c: AtomicTerm, _: NumberTerm, _)),
               body: AtomicTerm, _, _) =>
        assertEquals((t1.symbol.valid.map(_.text), t2.symbol.valid.map(_.text), f.f.text, c.f.text, body.f.text),
          (Some("f"), Some("c"), "f", "c", "$less"))
      case other => fail(s"unexpected $other")
    }
    formula("p($let(c: $int, c := 1, c))") match {
      case AtomicFormula(_, Seq(Let(Seq(_), Seq(_), _: AtomicTerm, _, _)), _, _) => ()
      case other                                                               => fail(s"unexpected $other")
    }
  }

  test("top-level ==, sequents and subtypes are statements; nested ones are Unsupported") {
    assert(statement("tff(a, axiom, f(a) == b).").isInstanceOf[Definition])
    statement("tff(a, axiom, [p, q] --> [r]).") match {
      case Sequent(Seq(_: AtomicFormula, _: AtomicFormula), Seq(_: AtomicFormula), _) => ()
      case other                                                                    => fail(s"unexpected $other")
    }
    assert(statement("tff(a, axiom, nat << int).").isInstanceOf[Subtype])
    assertEquals(unsupported("tff(a, axiom, p | (f(a) == b))."), Vector("txf_definition"))
  }
}
