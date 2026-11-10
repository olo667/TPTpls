package tptp.check

import tptp.semantics.{DiagnosticCode, Severity}
import tptp.syntax.{Span, SyntaxParser}
import tptp.syntax.tree.TffRecord

class TffCheckerSuite extends munit.FunSuite {
  private def record(text: String): TffRecord = SyntaxParser.parse(text).records.head match {
    case r: TffRecord => r
    case other        => fail(s"not a TFF record: $other")
  }
  private def local(text: String) = TffChecker.local(record(text))
  private def localCodes(text: String) = local(text).diagnostics.map(d => (d.code, d.message))
  private def declared(text: String): Ty = local(text).item match {
    case SummaryItem.Declaration(_, _, ty) => ty
    case other                             => fail(s"not a declaration: $other")
  }

  import Ty.*

  test("a type declaration is summarised with its structural type") {
    local("tff(f_type, type, f: (nat * list(T)) > $o).").item match {
      case SummaryItem.Declaration(Some(n), f, ty) =>
        assertEquals((n.text, f.text), ("f_type", "f"))
        assertEquals(ty, Fun(Vector(Con("nat", Vector.empty), Con("list", Vector(Unknown))), Bool))
      case other => fail(s"unexpected $other")
    }
    assertEquals(declared("tff(t, type, cons: !>[T: $tType]: (T > list(T)))."),
      Forall(Vector("T"), Fun(Vector(Var("T")), Con("list", Vector(Var("T"))))))
    assertEquals(declared("tff(t, type, pair: !>[A: $tType]: !>[B: $tType]: ((A * B) > p(A, B)))."),
      Forall(Vector("A", "B"), Fun(Vector(Var("A"), Var("B")), Con("p", Vector(Var("A"), Var("B"))))))
    assertEquals(declared("tff(t, type, nat: $tType)."), Kind)
    assertEquals(declared("tff(t, type, b: $oType)."), Bool)
  }

  test("a formula is summarised with its name and no uses") {
    local("tff(a, axiom, p(c)).").item match {
      case SummaryItem.Formula(Some(n), uses) => assertEquals((n.text, uses), ("a", Vector.empty))
      case other                              => fail(s"unexpected $other")
    }
  }

  test("roles: declarations need `type`, formulas must not have it") {
    assertEquals(localCodes("tff(a, axiom, f: $i > $i)."),
      Vector((DiagnosticCode.InvalidRole, "a type declaration needs role `type`")))
    assertEquals(localCodes("tff(a, type, p(c))."),
      Vector((DiagnosticCode.InvalidRole, "role `type` is only for type declarations")))
    assertEquals(localCodes("tff(a, axoim, p)."), Vector((DiagnosticCode.InvalidRole, "`axoim` is not a formula role")))
  }

  test("defined words: arithmetic and the TXF words are defined in TFF") {
    assertEquals(localCodes("tff(a, axiom, $less(1, $sum(2, 3)) & $true & $distinct(a, b) & p($ite(q, a, b)))."), Vector.empty)
    assertEquals(localCodes("tff(a, axiom, $lss(1, 2))."), Vector((DiagnosticCode.UnknownDefinedWord, "`$lss` is not a defined word")))
  }

  test("defined types as type arguments are defined words") {
    assertEquals(localCodes("tff(a, axiom, ! [A: $tType, M: map(A)] : p(get($int, A, M)))."), Vector.empty)
  }

  test("free and unused variables, as in FOF") {
    assertEquals(local("tff(a, axiom, ! [X: $int, Y: $int] : p(X, Z)).").diagnostics.map(d => (d.code, d.span)), Vector(
      (DiagnosticCode.FreeVariable, Span(42, 43)),
      (DiagnosticCode.UnusedVariable, Span(26, 27)),
    ))
  }

  test("type variables used in types and as type arguments count as used") {
    assertEquals(localCodes("tff(a, axiom, ! [T: $tType, X: T] : p(T, X))."), Vector.empty)
    assertEquals(localCodes("tff(a, axiom, ! [T: $tType, X: list(T)] : q(X))."), Vector.empty)
    assertEquals(localCodes("tff(t, type, f: !>[T: $tType]: (U > T))."),
      Vector((DiagnosticCode.FreeVariable, "type variable `U` is not bound by `!>`")))
  }

  test("TXF: $o variables as formulas, $let parameters bound in their definition") {
    assertEquals(localCodes("tff(a, axiom, ! [X: $o] : (X | p(X)))."), Vector.empty)
    assertEquals(localCodes("tff(a, axiom, $let(f: $int > $int, f(X) := $sum(X, 1), $less(f(1), 2)))."), Vector.empty)
    assertEquals(localCodes("tff(a, axiom, $let(f: $int > $int, f(X) := $sum(Y, 1), $less(f(1), 2))).").map(_._1),
      Vector(DiagnosticCode.FreeVariable))
  }

  test("a logic specification (NX0) is a valid record and is not checked") {
    assertEquals(localCodes("tff(l, logic, $modal == [$modalities == $modal_system_K])."), Vector.empty)
    assertEquals(localCodes("tff(l, logic, $modal == $modal_system_K)."), Vector.empty)
  }

  test("variables inside an Unsupported node are not checked") {
    assertEquals(localCodes("tff(a, axiom, ! [X] : (p(X) | {$box} @ (q(Y))))."), Vector.empty)
  }

  import java.nio.file.Path
  import tptp.semantics.Location
  import tptp.syntax.LineIndex

  private val here = (s: Span) => Location(Path.of("test.p"), s, LineIndex(""))

  /** A signature from declarations written in TFF, all visible, in one pretend file. */
  private def signature(decls: String*): Signature = Signature.of(decls.toVector.map(d => local(s"tff(d, type, $d).").item).collect {
    case SummaryItem.Declaration(_, s, ty) => Decl(s, ty, here(s.span))
  })
  private def typed(text: String, sig: Signature, axiomFile: Boolean = false) =
    TffChecker.typed(record(text), sig, axiomFile, here).map(p => (p.diagnostic.code, p.diagnostic.message))

  private val nat = signature("nat: $tType", "list: $tType > $tType", "zero: nat", "s: nat > nat", "even: nat > $o",
    "cons: !>[T: $tType]: ((T * list(T)) > list(T))", "color: nat > $o", "color: (nat * nat) > $o",
    "pair: !>[A: $tType]: !>[B: $tType]: ((A * B) > nat)")

  test("arguments are checked against the declared types") {
    assertEquals(typed("tff(a, axiom, even(s(zero))).", nat), Vector.empty)
    assertEquals(typed("tff(a, axiom, even(1)).", nat),
      Vector((DiagnosticCode.TypeMismatch, "argument 1 of `even` has type `$int`, but `even` expects `nat`")))
  }

  test("a function used as a formula clashes; a type used as a term is a mismatch") {
    assertEquals(typed("tff(a, axiom, s(zero)).", nat).map(_._1), Vector(DiagnosticCode.PredicateFunctionClash))
    assertEquals(typed("tff(a, axiom, even(nat)).", nat),
      Vector((DiagnosticCode.TypeMismatch, "`nat` is a type, but used as a term here")))
  }

  test("overloads are resolved by argument count") {
    assertEquals(typed("tff(a, axiom, color(zero) & color(zero, zero)).", nat), Vector.empty)
    assertEquals(typed("tff(a, axiom, color(zero, zero, zero)).", nat),
      Vector((DiagnosticCode.ArityMismatch, "`color` is declared with 1 or 2 arguments, but has 3")))
  }

  test("TF1 instantiation: type arguments first, then the instantiated argument types") {
    assertEquals(typed("tff(a, axiom, ! [L: list(nat)] : cons(nat, zero, L) = L).", nat), Vector.empty)
    assertEquals(typed("tff(a, axiom, ! [L: list($int)] : cons($int, 1, L) = L).", nat), Vector.empty)
    assertEquals(typed("tff(a, axiom, ! [L: list(nat)] : cons(nat, 1, L) = L).", nat).map(_._1), Vector(DiagnosticCode.TypeMismatch))
    assertEquals(typed("tff(a, axiom, ! [L: list(nat)] : cons(zero, L) = L).", nat),
      Vector((DiagnosticCode.WrongTypeArguments, "`cons` takes 1 type argument, but has 0")))
    assertEquals(typed("tff(a, axiom, ! [T: $tType, X: T, L: list(T)] : cons(T, X, L) = L).", nat), Vector.empty)
    assertEquals(typed("tff(a, axiom, pair(nat, $int, zero, 1) = zero).", nat), Vector.empty)
    assertEquals(typed("tff(a, axiom, ! [L: list(nat)] : cons(fluent, zero, L) = L).", nat).map(_._1),
      Vector(DiagnosticCode.UndeclaredType))
  }

  test("undeclared symbols take default types in problem files and are warnings in axiom files") {
    assertEquals(typed("tff(a, axiom, q(zero)).", nat),
      Vector((DiagnosticCode.TypeMismatch, "argument 1 of `q` has type `nat`, but `q` is not declared, so it expects `$i`")))
    assertEquals(typed("tff(a, axiom, q(zero)).", nat, axiomFile = true),
      Vector((DiagnosticCode.UndeclaredSymbol, "`q` is not declared in this file or its includes")))
  }

  test("an undeclared symbol in a term position is `$i`, or a `$o` value (TXF uses undeclared predicates as Booleans)") {
    val sig = signature("nat: $tType", "zero: nat", "barbar: ($o * $o) > $o")
    assertEquals(typed("tff(a, axiom, ! [X, Y] : (barbar(eqNat(X, Y), q) <=> eqNat(X, Y))).", sig), Vector.empty)
    assertEquals(typed("tff(a, axiom, barbar(eqNat(zero), q)).", sig).map(_._1), Vector(DiagnosticCode.TypeMismatch))
    assertEquals(typed("tff(a, axiom, even(zeor)).", nat),
      Vector((DiagnosticCode.TypeMismatch, "argument 1 of `even` has type `$i`, but `even` expects `nat`")))
    assertEquals(typed("tff(a, axiom, zeor = zero).", nat),
      Vector((DiagnosticCode.TypeMismatch, "the sides of `=` have types `$i` and `nat`")))
  }

  test("a use before the symbol's declaration in the same part is an error") {
    val later = new Signature {
      def apply(key: String) = Vector.empty
      def later(key: String) = nat(key).headOption
    }
    assertEquals(typed("tff(a, axiom, even(zero)).", later).map(_._2).headOption, Some("`even` is used before its declaration"))
  }

  test("undeclared types: an error in problem files, a warning in axiom files, and no cascade") {
    assertEquals(typed("tff(a, axiom, ! [F: fluent] : even(F)).", nat),
      Vector((DiagnosticCode.UndeclaredType, "type `fluent` is not declared")))
    val fluent = signature("g: fluent > $o", "k: $i")
    assertEquals(typed("tff(a, axiom, g(k)).", fluent), Vector.empty, "a declaration with an undeclared type")
    val ax = TffChecker.typed(record("tff(a, axiom, ! [F: fluent] : even(F))."), nat, axiomFile = true, here).map(_.diagnostic)
    assertEquals(ax.map(d => (d.code, d.severity, d.message)),
      Vector((DiagnosticCode.UndeclaredType, Severity.Warning, "type `fluent` is not declared in this file or its includes")))
  }

  test("type constructors get their declared number of arguments") {
    assertEquals(typed("tff(a, axiom, ! [L: list] : p(L)).", nat),
      Vector((DiagnosticCode.WrongTypeArguments, "type `list` takes 1 argument, but has 0")))
  }

  test("malformed type binders and kinds") {
    assertEquals(typed("tff(t, type, f: !>[T]: (T > T)).", nat).map(_._2),
      Vector("type variable `T` must have type `$tType`"))
    assertEquals(typed("tff(t, type, f: !>[T: $i]: ($i > $i)).", nat).map(_._2),
      Vector("type variable `T` must have type `$tType`"))
    assertEquals(typed("tff(a, axiom, ? [T: $tType] : $true).", nat).map(_._2),
      Vector("type variables are bound by `!`, not `?`"))
    assertEquals(typed("tff(t, type, f: $tType > $i).", nat).map(_._2),
      Vector("a type constructor maps `$tType`s to `$tType`"))
  }

  test("equality sides have one type") {
    assertEquals(typed("tff(a, axiom, zero = 1).", nat),
      Vector((DiagnosticCode.TypeMismatch, "the sides of `=` have types `nat` and `$int`")))
  }

  test("arithmetic: numbers typed by form, one numeric type per application") {
    assertEquals(typed("tff(a, axiom, $less($sum(1, 2), 3) & $is_rat(1/2) & $to_int(2.5) = 2).", nat), Vector.empty)
    assertEquals(typed("tff(a, axiom, $less(1, 2.0)).", nat),
      Vector((DiagnosticCode.TypeMismatch, "argument 2 of `$less` has type `$real`, but `$less` expects `$int` like argument 1")))
    assertEquals(typed("tff(a, axiom, $quotient(1, 2) = 0).", nat).map(_._1), Vector(DiagnosticCode.TypeMismatch))
    assertEquals(typed("tff(a, axiom, $less(zero, 1)).", nat).map(_._1), Vector(DiagnosticCode.TypeMismatch))
  }

  test("a record with an Unsupported node is not type-checked") {
    assertEquals(typed("tff(a, axiom, even(1) | {$box} @ (p)).", nat), Vector.empty)
  }

  test("syntax errors do not cascade into type errors") {
    def problems(text: String) = SyntaxParser.parse(text).records.head match {
      case r: TffRecord => TffChecker.typed(r, nat, axiomFile = false, here).map(_.diagnostic.code)
      case other        => fail(s"unexpected $other")
    }
    assertEquals(problems("tff(a, axiom, even(zero, ))."), Vector.empty)
    assertEquals(problems("tff(a, axiom, even(s(@ @ @))) )."), Vector.empty)
    assertEquals(problems("tff(f_type, type, f: (nat * ) > nat)."), Vector.empty)
    assertEquals(problems("tff(f_type, type, f: !>[T: ]: (T > $o))."), Vector.empty)
    assertEquals(localCodes("tff(f_type, type, ).").filter(_._1 == DiagnosticCode.InvalidRole), Vector.empty)
  }

  test("$o values: variables, formulas as terms, $true and predicates as arguments") {
    val sig = signature("nat: $tType", "says: (nat * $o) > $o", "even: nat > $o", "zero: nat")
    assertEquals(typed("tff(a, axiom, ! [P: nat, X: $o] : (says(P, $true) <~> says(P, X) | X | says(P, even(P) & X))).", sig),
      Vector.empty)
    assertEquals(typed("tff(a, axiom, ! [P: nat] : P).", sig),
      Vector((DiagnosticCode.TypeMismatch, "variable `P` has type `nat`, but is used as a formula")))
    assertEquals(typed("tff(a, axiom, says(zero, zero)).", sig).map(_._1), Vector(DiagnosticCode.TypeMismatch))
  }

  test("$ite: a $o condition and branches of one type") {
    assertEquals(typed("tff(a, axiom, s($ite(even(zero), zero, s(zero))) = zero).", nat), Vector.empty)
    assertEquals(typed("tff(a, axiom, $ite(even(zero), even(zero), $false)).", nat), Vector.empty)
    assertEquals(typed("tff(a, axiom, s($ite(even(zero), zero, 1)) = zero).", nat).map(_._1), Vector(DiagnosticCode.TypeMismatch))
    assertEquals(typed("tff(a, axiom, s($ite(zero, zero, zero)) = zero).", nat).map(_._1), Vector(DiagnosticCode.TypeMismatch))
  }

  test("$distinct: arguments of one type") {
    assertEquals(typed("tff(a, axiom, $distinct(zero, s(zero))).", nat), Vector.empty)
    assertEquals(typed("tff(a, axiom, $distinct(zero, 1)).", nat).map(_._1), Vector(DiagnosticCode.TypeMismatch))
  }

  test("tuples have the tuple of their element types") {
    val sig = signature("nat: $tType", "zero: nat", "p: [nat, $int] > $o")
    assertEquals(typed("tff(a, axiom, p([zero, 1])).", sig), Vector.empty)
    assertEquals(typed("tff(a, axiom, p([1, zero])).", sig).map(_._1), Vector(DiagnosticCode.TypeMismatch))
  }

  test("$let typings shadow the signature in the definitions and the body only") {
    val sig = signature("nat: $tType", "c: nat", "even: nat > $o")
    assertEquals(typed("tff(a, axiom, $let(c: $int, c := 1, $less(c, 2)) & even(c)).", sig), Vector.empty)
    assertEquals(typed("tff(a, axiom, $let(f: $int > $int, f(X) := $sum(X, 1), $less(f(1), 2))).", sig), Vector.empty)
    assertEquals(typed("tff(a, axiom, $let(f: $int > $int, f(X) := c, $less(f(1), 2))).", sig),
      Vector((DiagnosticCode.TypeMismatch, "the right-hand side of `f` has type `nat`, but `f` has type `$int`")))
    assertEquals(typed("tff(a, axiom, $let([a: $int, b: $int], [a, b] := [1, 2], $less(a, b))).", sig), Vector.empty)
  }

  test("malformed $let definitions") {
    assertEquals(typed("tff(a, axiom, $let(f: $int > $int, g(X) := X, $true)).", nat).map(_._2),
      Vector("`g` is not typed in this `$let`"))
    assertEquals(typed("tff(a, axiom, $let(f: ($int * $int) > $int, f(X, X) := X, $true)).", nat).map(_._2),
      Vector("`X` appears twice on the left-hand side"))
    assertEquals(typed("tff(a, axiom, $let(f: $int > $int, f(1) := 2, $true)).", nat).map(_._2),
      Vector("the left-hand side must be a symbol applied to variables, or a tuple of constants"))
  }

  test("== and subtypes") {
    assertEquals(typed("tff(a, axiom, s(zero) == zero).", nat), Vector.empty)
    assertEquals(typed("tff(a, axiom, s(zero) == 1).", nat).map(_._1), Vector(DiagnosticCode.TypeMismatch))
    assertEquals(typed("tff(a, axiom, nat << int).", nat).map(_._1), Vector(DiagnosticCode.UndeclaredType))
  }
}
