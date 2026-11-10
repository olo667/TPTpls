package tptp.check

import Ty.*

class TyKindsSuite extends munit.FunSuite {
  private val nat = Con("nat", Vector.empty)
  private def list(t: Ty) = Con("list", Vector(t))

  test("types print as written in TPTP") {
    assertEquals(show(Fun(Vector(nat, list(nat)), Bool)), "(nat * list(nat)) > $o")
    assertEquals(show(Fun(Vector(nat), nat)), "nat > nat")
    assertEquals(show(Forall(Vector("T"), Fun(Vector(Var("T")), list(Var("T"))))), "!>[T: $tType]: T > list(T)")
    assertEquals(show(Tuple(Vector(nat, Individual))), "[nat, $i]")
    assertEquals(show(Kind), "$tType")
  }

  test("type constructors and tuples are compatible element by element") {
    assert(compatible(list(nat), list(Unknown)))
    assert(!compatible(list(nat), list(Individual)))
    assert(!compatible(list(nat), Con("set", Vector(nat))))
    assert(!compatible(Tuple(Vector(nat)), Tuple(Vector(nat, nat))))
  }

  test("polymorphic types are equivalent up to renaming, with nested quantifiers flattened") {
    val a = Forall(Vector("T"), Fun(Vector(Var("T")), list(Var("T"))))
    assert(equivalent(a, Forall(Vector("U"), Fun(Vector(Var("U")), list(Var("U"))))))
    assert(!equivalent(a, Forall(Vector("U"), Fun(Vector(Var("U")), list(nat)))))
    assertEquals(forall(Vector("A"), forall(Vector("B"), Fun(Vector(Var("A")), Var("B")))),
      Forall(Vector("A", "B"), Fun(Vector(Var("A")), Var("B"))))
  }

  test("substitution replaces type variables, and arity counts type arguments") {
    val cons = Forall(Vector("T"), Fun(Vector(Var("T"), list(Var("T"))), list(Var("T"))))
    assertEquals(arity(cons), Some(3))
    assertEquals(substitute(Fun(Vector(Var("T"), list(Var("T"))), list(Var("T"))), Map("T" -> nat)),
      Fun(Vector(nat, list(nat)), list(nat)))
  }
}
