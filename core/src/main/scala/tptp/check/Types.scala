package tptp.check

import tptp.semantics.{Diagnostic, Location}
import tptp.syntax.tree.{Include, Name}

/** Types of FOF and TFF (TF0, TF1, TXF) symbols and terms. */
enum Ty {
  case Con(name: String, args: Vector[Ty])     // $i, $o, $int, $rat, $real, nat, list($int)
  case Var(name: String)                       // a TF1 type variable
  case Fun(args: Vector[Ty], result: Ty)       // (t1 * … * tn) > r
  case Forall(vars: Vector[String], body: Ty)  // !>[T1, …]: body (never nested: see `forall`)
  case Tuple(elements: Vector[Ty])             // [t1, …, tn]
  case Kind                                    // $tType
  case Partial(result: Ty)                     // FOF: returns `result`; its arguments are unknown
  case Default                                 // an undeclared TFF symbol as a term: `$i`, or a `$o` value (TXF)
  case Unknown                                 // from an error or an unchecked construct: compatible with anything
}

object Ty {
  val Individual: Ty = Con("$i", Vector.empty)
  val Bool: Ty = Con("$o", Vector.empty)

  /** `!>[vars]: body`, with a nested `!>` merged into one quantifier, as Leo does. */
  def forall(vars: Vector[String], body: Ty): Ty = body match {
    case Forall(more, inner) => Forall(vars ++ more, inner)
    case _ if vars.isEmpty   => body
    case _                   => Forall(vars, body)
  }

  /** The type of an atom or term with these argument types; a constant has just its result type. */
  def applied(args: Vector[Ty], result: Ty): Ty = if (args.isEmpty) result else Fun(args, result)

  def result(t: Ty): Ty = t match {
    case Fun(_, r)       => r
    case Partial(r)      => r
    case Forall(_, body) => result(body)
    case other           => other
  }

  /** Whether a declaration of this type declares a type (a type constant or constructor). */
  def isType(t: Ty): Boolean = result(t) == Kind

  /** The argument count, if known; a polymorphic symbol's type arguments count. */
  def arity(t: Ty): Option[Int] = t match {
    case Fun(args, _)                         => Some(args.size)
    case Forall(vs, body)                     => arity(body).map(_ + vs.size)
    case Partial(_) | Unknown                 => None
    case Con(_, _) | Var(_) | Tuple(_) | Kind | Default => Some(0)
  }

  /** Whether `t` has no `Unknown` part, e.g. from a syntax error. */
  def complete(t: Ty): Boolean = t match {
    case Unknown          => false
    case Con(_, args)     => args.forall(complete)
    case Fun(args, r)     => args.forall(complete) && complete(r)
    case Forall(_, body)  => complete(body)
    case Tuple(es)        => es.forall(complete)
    case Partial(r)       => complete(r)
    case Var(_) | Kind | Default => true
  }

  /** Whether the arity and result are known, so the type can serve as a symbol's first use. */
  def known(t: Ty): Boolean = arity(t).isDefined

  /** Whether `a` and `b` can be types of the same symbol or term: equal up to `Unknown` and `Partial`. */
  def compatible(a: Ty, b: Ty): Boolean = (a, b) match {
    case (Unknown, _) | (_, Unknown) => true
    case (Default, t)                => t == Individual || t == Bool || t == Default
    case (t, Default)                => t == Individual || t == Bool
    case (Partial(r), t)             => compatible(r, result(t))
    case (t, Partial(r))             => compatible(result(t), r)
    case (Fun(as, r1), Fun(bs, r2))  => as.size == bs.size && as.zip(bs).forall(compatible) && compatible(r1, r2)
    case (Con(n1, as), Con(n2, bs))  => n1 == n2 && as.size == bs.size && as.zip(bs).forall(compatible)
    case (Tuple(as), Tuple(bs))      => as.size == bs.size && as.zip(bs).forall(compatible)
    case (f1: Forall, f2: Forall)    => equivalent(f1, f2)
    case _                           => a == b
  }

  /** Equal up to renaming the variables of `Forall`s. */
  def equivalent(a: Ty, b: Ty): Boolean = (a, b) match {
    case (Forall(v1, b1), Forall(v2, b2)) =>
      v1.size == v2.size && equivalent(b1, substitute(b2, v2.zip(v1.map(Var(_))).toMap))
    case _ => a == b
  }

  /** Replaces free type variables. */
  def substitute(t: Ty, s: Map[String, Ty]): Ty = t match {
    case Var(n)           => s.getOrElse(n, t)
    case Con(n, args)     => Con(n, args.map(substitute(_, s)))
    case Fun(args, r)     => Fun(args.map(substitute(_, s)), substitute(r, s))
    case Forall(vs, body) => Forall(vs, substitute(body, s -- vs))
    case Tuple(es)        => Tuple(es.map(substitute(_, s)))
    case Partial(r)       => Partial(substitute(r, s))
    case Kind | Unknown | Default => t
  }

  /** TPTP notation: `(nat * list(nat)) > $o`, `!>[T: $tType]: T > T`, `[nat, $i]`. */
  def show(t: Ty): String = t match {
    case Con(n, Vector())  => n
    case Con(n, args)      => s"$n(${args.map(show).mkString(", ")})"
    case Var(n)            => n
    case Kind              => "$tType"
    case Tuple(es)         => s"[${es.map(show).mkString(", ")}]"
    case Fun(Vector(a), r) => s"${operand(a)} > ${show(r)}"
    case Fun(args, r)      => s"(${args.map(operand).mkString(" * ")}) > ${show(r)}"
    case Forall(vs, body)  => s"!>[${vs.map(v => s"$v: $$tType").mkString(", ")}]: ${show(body)}"
    case Partial(r)        => s"(…) > ${show(r)}"
    case Unknown           => "?"
    case Default           => "$i"
  }

  private def operand(t: Ty): String = t match {
    case _: Fun | _: Forall => s"(${show(t)})"
    case other              => show(other)
  }
}

/** One use of a plain or system symbol, with the type this use implies. */
final case class Use(name: Name, ty: Ty)

/** A file's records in source order, as the problem-level check needs them. */
enum SummaryItem {
  case Formula(name: Option[Name], uses: Vector[Use])         // a FOF record (TFF: no uses)
  case Declaration(name: Option[Name], symbol: Name, ty: Ty)  // a TFF type record; `name` is the record's name
  case IncludeAt(include: Include)                       // where an included file's records go
}

final case class FileSummary(items: Vector[SummaryItem])

/** A TFF declaration visible to the typed walk. */
final case class Decl(symbol: Name, ty: Ty, at: Location)

/** The declarations a record sees: those of a key before it, in problem order; and the first one after it in the same
  * part of the problem (the open file, or the same include), to tell "used before its declaration". */
trait Signature {
  def apply(key: String): Vector[Decl]
  def later(key: String): Option[Decl]
}

object Signature {
  /** All of `decls` visible, nothing later. */
  def of(decls: Vector[Decl]): Signature = new Signature {
    private val byKey = decls.groupBy(_.symbol.key)
    def apply(key: String): Vector[Decl] = byKey.getOrElse(key, Vector.empty)
    def later(key: String): Option[Decl] = None
  }
}

/** A type error, with what it is about (`subject`: a symbol, type or variable) and a few words for include-line
  * summaries (`brief`, e.g. "argument 1: `$int` vs `$i`"). */
final case class TypeProblem(subject: String, brief: String, diagnostic: Diagnostic)

/** What checking one record yields. */
final case class RecordCheck[+I <: SummaryItem](item: I, diagnostics: Vector[Diagnostic])
