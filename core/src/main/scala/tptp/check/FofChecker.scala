package tptp.check

import scala.collection.mutable
import tptp.semantics.{Diagnostic, DiagnosticCode, Severity}
import tptp.syntax.{ErrorCause, Span}
import tptp.syntax.languages.Fof
import tptp.syntax.tree.*

/** Checks one FOF record: its symbol uses (for the problem-level check) and its local diagnostics. */
object FofChecker {
  /** A scope key no variable can have: marks a scope below a quantifier with a broken variable list. */
  private val Opaque = "?opaque"

  def check(r: FofRecord): RecordCheck = {
    val walk = new Walk(r.errors.map(_.span))
    role(r.role).foreach(walk.diagnostics += _)
    r.statement match {
      case Logical(f, _)     => walk.formula(f, Map.empty)
      case Sequent(l, rs, _) => (l ++ rs).foreach(walk.formula(_, Map.empty))
    }
    RecordCheck(SummaryItem.Formula(r.name.valid, walk.uses.result()), walk.diagnostics.result())
  }

  private def role(r: Role): Option[Diagnostic] = r.word.valid.flatMap { w =>
    val message =
      if (Fof.roles(w.text)) None
      else if (w.text == "unknown") Some("role `unknown` marks an error situation")
      else if (Fof.bnfRoles(w.text)) Some(s"role `${w.text}` is not used in FOF")
      else Some(s"`${w.text}` is not a formula role")
    message.map(Diagnostic(w.span, Severity.Error, DiagnosticCode.InvalidRole, _))
  }

  /** A quantified variable; `used` is set when a use in its scope refers to it. A quantifier whose variable list
    * has a syntax error binds unknown names: `opaque` bindings stop free-variable reports below them. */
  private final class Binding(val name: Name) { var used = false }

  /** `errors`: the record's syntax error spans. ANTLR's inline recovery (a dropped or inserted token) leaves no
    * ErrorNode, so these spans are the only trace; uses and scopes they touch are treated as unknown. */
  private final class Walk(errors: Vector[Span]) {
    val uses = Vector.newBuilder[Use]
    val diagnostics = Vector.newBuilder[Diagnostic]

    def formula(f: Formula, scope: Map[String, Binding]): Unit = f match {
      case a: AtomicFormula =>
        if (a.kind == AtomKind.Defined)
          definedWord(a.f, if (a.args.isEmpty) Fof.definedPropositions else Fof.definedPredicates)
        else uses += Use(a.f, applied(a.span, a.args, Ty.Bool))
        a.args.foreach(term(_, scope))
      case Equality(l, r, _, _)   => term(l, scope); term(r, scope)
      case Inequality(l, r, _, _) => term(l, scope); term(r, scope)
      case q @ QuantifiedFormula(_, vars, body, _, _) =>
        val bindings = vars.flatMap(_.valid).map(n => new Binding(n))
        val varList = Span(q.span.start, math.max(q.span.start, body.outer.start))
        val opaque = errors.exists(e => within(e, varList))
        formula(body, scope ++ bindings.map(b => b.name.key -> b) ++ (if (opaque) Map(Opaque -> new Binding(Name(Opaque, q.span))) else Map.empty))
        // a later binding of the same name in one list shadows the earlier one
        val effective = bindings.groupBy(_.name.key).values.map(_.last).toSet
        if (!containsError(body) && !errors.exists(e => within(e, body.outer)))
          bindings.filter(b => !b.used || !effective(b)).foreach { b =>
            diagnostics += Diagnostic(b.name.span, Severity.Warning, DiagnosticCode.UnusedVariable,
              s"variable `${b.name.text}` is never used")
          }
      case UnaryFormula(_, body, _, _)  => formula(body, scope)
      case BinaryFormula(_, l, r, _, _) => formula(l, scope); formula(r, scope)
      case _: ErrorNode                 => ()
    }

    def term(t: Term, scope: Map[String, Binding]): Unit = t match {
      case a: AtomicTerm =>
        if (a.kind == AtomKind.Defined) definedWord(a.f, Fof.definedFunctors)
        else uses += Use(a.f, applied(a.span, a.args, Ty.Individual))
        a.args.foreach(term(_, scope))
      case Variable(name, _) =>
        scope.get(name.key) match {
          case Some(b) => b.used = true
          case None if scope.contains(Opaque) => ()
          case None =>
            diagnostics += Diagnostic(name.span, Severity.Error, DiagnosticCode.FreeVariable,
              s"variable `${name.text}` is not bound by a quantifier")
        }
      case _: DistinctObject | _: NumberTerm | _: ErrorNode => ()
    }

    private def definedWord(f: Name, allowed: Set[String]): Unit =
      if (!allowed(f.text)) {
        val message =
          if (Fof.bnfDefinedWords(f.text)) s"`${f.text}` is not defined in FOF" else s"`${f.text}` is not a defined word"
        diagnostics += Diagnostic(f.span, Severity.Warning, DiagnosticCode.UnknownDefinedWord, message)
      }

    /** A hole is one argument of unknown type; an unparsable or incomplete argument list, or a syntax error in the
      * argument list outside the arguments themselves (a dropped `,`), makes the arity unknown. */
    private def applied(span: Span, args: Seq[Term], result: Ty): Ty = {
      // the argument list starts at the first argument's `(`; a dropped `,` lies inside it but in no argument
      val argList = Span(math.max(span.start, args.headOption.fold(span.end)(_.span.start) - 1), span.end)
      val droppedToken = args.nonEmpty && errors.exists(e => within(e, argList) && !args.exists(a => within(e, a.span)))
      val types = args.toVector.map {
        case ErrorNode(ErrorCause.MissingElement(_), _, _, _) => Some(Ty.Unknown)
        case _: ErrorNode                                      => None
        case _                                                 => Some(Ty.Individual)
      }
      if (droppedToken || types.contains(None)) Ty.Partial(result) else Ty.applied(types.flatten, result)
    }

    private def within(e: Span, s: Span): Boolean = e.start >= s.start && e.end <= s.end

    private def containsError(t: Tree): Boolean = t match {
      case _: ErrorNode => true
      case other        => other.children.exists(containsError)
    }
  }
}
