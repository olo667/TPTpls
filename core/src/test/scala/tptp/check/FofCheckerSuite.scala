package tptp.check

import tptp.syntax.SyntaxParser
import tptp.syntax.tree.FofRecord

class FofCheckerSuite extends munit.FunSuite {
  private def check(text: String): RecordCheck = SyntaxParser.parse(text).records.head match {
    case r: FofRecord => FofChecker.check(r)
    case other        => fail(s"not a FOF record: $other")
  }
  private def uses(text: String): Vector[(String, Ty)] = check(text).item.uses.map(u => (u.name.text, u.ty))

  import Ty.*

  test("atoms are predicates and terms are functions over individuals") {
    assertEquals(uses("fof(a, axiom, p(f(c), X) | q)."), Vector(
      "p" -> Fun(Vector(Individual, Individual), Bool),
      "f" -> Fun(Vector(Individual), Individual),
      "c" -> Individual,
      "q" -> Bool,
    ))
  }

  test("defined words, variables, numbers, distinct objects and equality are not uses") {
    assertEquals(uses("fof(a, axiom, ! [X] : ($true & X = 1 & X != \"o\"))."), Vector.empty)
  }

  test("system symbols are uses") {
    assertEquals(uses("fof(a, axiom, $$p($$c))."), Vector("$$p" -> Fun(Vector(Individual), Bool), "$$c" -> Individual))
  }

  test("a hole argument keeps the arity; a collapsed argument list makes it unknown") {
    assertEquals(uses("fof(a, axiom, p(X, ))."), Vector("p" -> Fun(Vector(Individual, Unknown), Bool)))
    assertEquals(uses("fof(a, axiom, p(f(@ @ @ @), b)).").toMap.get("f"), Some(Partial(Individual)))
  }

  test("the record's name is taken unless it is an error") {
    assertEquals(check("fof(a, axiom, p).").item.name.map(_.text), Some("a"))
    assertEquals(check("fof(, axiom, p).").item.name, None)
  }

  test("compatible types are equal up to unknown and partial ones") {
    assert(compatible(Fun(Vector(Individual, Unknown), Bool), Fun(Vector(Individual, Individual), Bool)))
    assert(compatible(Partial(Bool), Fun(Vector(Individual), Bool)))
    assert(!compatible(Partial(Bool), Individual))
    assert(!compatible(Fun(Vector(Individual), Bool), Fun(Vector(Individual, Individual), Bool)))
    assert(!compatible(Bool, Fun(Vector(Individual), Bool)))
  }

  import tptp.semantics.{DiagnosticCode, Severity}
  import tptp.syntax.Span

  private def diagnostics(text: String) = check(text).diagnostics.map(d => (d.code, d.severity, d.message, d.span))

  test("a variable not bound by a quantifier is free") {
    assertEquals(diagnostics("fof(a, axiom, p(X))."),
      Vector((DiagnosticCode.FreeVariable, Severity.Error, "variable `X` is not bound by a quantifier", Span(16, 17))))
  }

  test("a quantified variable that is never used is reported on its binding") {
    assertEquals(diagnostics("fof(a, axiom, ! [X, Y] : p(X))."),
      Vector((DiagnosticCode.UnusedVariable, Severity.Warning, "variable `Y` is never used", Span(20, 21))))
  }

  test("inner bindings shadow outer ones") {
    assertEquals(diagnostics("fof(a, axiom, ! [X] : ? [X] : p(X)).").map(d => (d._1, d._4)),
      Vector((DiagnosticCode.UnusedVariable, Span(17, 18))))
  }

  test("a broken body suppresses the unused-variable warning, and errors raise no free variables") {
    assertEquals(diagnostics("fof(a, axiom, ! [X] : (q & ))."), Vector.empty)
    assertEquals(diagnostics("fof(a, axiom, ! [X] : (p(X) & ))."), Vector.empty)
  }

  test("roles: valid, not used in FOF, an error situation, unknown") {
    assertEquals(diagnostics("fof(a, conjecture-x, p)."), Vector.empty)
    assertEquals(diagnostics("fof(a, axoim, p).").map(_._3), Vector("`axoim` is not a formula role"))
    assertEquals(diagnostics("fof(a, type, p).").map(_._3), Vector("role `type` is not used in FOF"))
    assertEquals(diagnostics("fof(a, unknown, p).").map(d => (d._1, d._3)),
      Vector((DiagnosticCode.InvalidRole, "role `unknown` marks an error situation")))
  }

  test("$-words: defined in FOF, defined elsewhere, unknown") {
    assertEquals(diagnostics("fof(a, axiom, $true | ~ $false)."), Vector.empty)
    assertEquals(diagnostics("fof(a, axiom, $less(c, d)).").map(d => (d._1, d._2, d._3)),
      Vector((DiagnosticCode.UnknownDefinedWord, Severity.Warning, "`$less` is not defined in FOF")))
    assertEquals(diagnostics("fof(a, axiom, $tru).").map(_._3), Vector("`$tru` is not a defined word"))
    assertEquals(diagnostics("fof(a, axiom, p($true)).").map(_._3), Vector("`$true` is not defined in FOF"))
  }

  test("a dropped token in a variable list or argument list raises no type or variable errors") {
    assertEquals(diagnostics("fof(a, axiom, ! [X Y] : p(X,Y))."), Vector.empty)
    assertEquals(uses("fof(a, axiom, p(a b)).").toMap.get("p"), Some(Partial(Bool)))
    assertEquals(diagnostics("fof(a, axiom, ! [X,Y] : p(X Y))."), Vector.empty)
    assertEquals(uses("fof(a, axiom, p(f(a b)))."), Vector("p" -> Fun(Vector(Individual), Bool), "f" -> Partial(Individual), "a" -> Individual))
  }
}
