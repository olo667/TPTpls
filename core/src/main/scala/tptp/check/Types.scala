package tptp.check

import tptp.semantics.Diagnostic
import tptp.syntax.tree.{Include, Name}

/** A small type language: enough for FOF, extended for TFF/THF later (user types, $int…, $tType, type variables). */
enum Ty {
  case Individual                         // $i
  case Bool                               // $o
  case Fun(args: Vector[Ty], result: Ty)  // (t1 * … * tn) > r
  case Partial(result: Ty)                // returns `result`; its arguments are unknown
  case Unknown                            // from an ErrorNode: compatible with anything
}

object Ty {
  /** The type of an atom or term with these argument types; a constant has just its result type. */
  def applied(args: Vector[Ty], result: Ty): Ty = if (args.isEmpty) result else Fun(args, result)

  def result(t: Ty): Ty = t match {
    case Fun(_, r)  => r
    case Partial(r) => r
    case other      => other
  }

  /** The argument count, if known. */
  def arity(t: Ty): Option[Int] = t match {
    case Fun(args, _)          => Some(args.size)
    case Partial(_) | Unknown  => None
    case Individual | Bool     => Some(0)
  }

  /** Whether the arity and result are known, so the type can serve as a symbol's first use. */
  def known(t: Ty): Boolean = arity(t).isDefined

  /** Whether `a` and `b` can be types of the same symbol: equal up to `Unknown` and `Partial`. */
  def compatible(a: Ty, b: Ty): Boolean = (a, b) match {
    case (Unknown, _) | (_, Unknown)    => true
    case (Partial(r), t)                => compatible(r, result(t))
    case (t, Partial(r))                => compatible(result(t), r)
    case (Fun(as, r1), Fun(bs, r2))     => as.size == bs.size && as.zip(bs).forall(compatible) && compatible(r1, r2)
    case _                              => a == b
  }
}

/** One use of a plain or system symbol, with the type this use implies. */
final case class Use(name: Name, ty: Ty)

/** A file's records in source order, as the problem-level check needs them. */
enum SummaryItem {
  case Formula(name: Option[Name], uses: Vector[Use])   // a FOF record (later: CNF, TFF, …)
  case IncludeAt(include: Include)                       // where an included file's records go
}

final case class FileSummary(items: Vector[SummaryItem])

/** What checking one record yields. */
final case class RecordCheck(item: SummaryItem.Formula, diagnostics: Vector[Diagnostic])
