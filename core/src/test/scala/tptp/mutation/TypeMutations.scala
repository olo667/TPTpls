package tptp.mutation

import java.nio.file.Path
import scala.util.Random
import tptp.analysis.{DiskSources, FileAnalysis, ProjectAnalyzer, SourceProvider, Stamp}
import tptp.check.{SummaryItem, TffChecker, Ty}
import tptp.syntax.languages.Tff
import tptp.semantics.*
import tptp.syntax.Span
import tptp.syntax.tree.*

/** A mutation that should, or (for controls) should not, make the checker report an error. */
enum TypeMutationKind(val label: String, val expected: Option[DiagnosticCode]) {
  case KindSwap extends TypeMutationKind("kind swap", Some(DiagnosticCode.PredicateFunctionClash))
  case AritySwap extends TypeMutationKind("arity swap", Some(DiagnosticCode.ArityMismatch))
  case SameType extends TypeMutationKind("same type (control)", None)
  case Unbind extends TypeMutationKind("unbind variable", Some(DiagnosticCode.FreeVariable))
  case Rebind extends TypeMutationKind("rebind variable (control)", None)
  case TffSymbolSwap extends TypeMutationKind("TFF symbol swap", Some(DiagnosticCode.TypeMismatch))
  case TffSameType extends TypeMutationKind("TFF same type (control)", None)
  case NumberRetype extends TypeMutationKind("number retype", Some(DiagnosticCode.TypeMismatch))
  case VariableRetype extends TypeMutationKind("variable retype", Some(DiagnosticCode.TypeMismatch))

  /** TFF kinds: a swap changes what the walk expects further out, so the error may be any type error, anywhere around. */
  def tff: Boolean = this == TffSymbolSwap || this == NumberRetype || this == VariableRetype
}

/** A name in the root file replaced by `replacement`; `at` is where the new name starts. */
final case class TypeMutant(kind: TypeMutationKind, description: String, text: String, at: Int)

enum TypePrecision { case Exact, Displaced, Missed, Clean }

final case class TypeOutcome(file: String, mutant: TypeMutant, precision: TypePrecision, violations: Vector[String])

/** Type mutants of a valid problem: symbol and variable names swapped for others from the same problem. */
object TypeMutations {
  private val checkerErrors: Set[DiagnosticCode] = Set(DiagnosticCode.ArityMismatch, DiagnosticCode.PredicateFunctionClash,
    DiagnosticCode.ConflictsInInclude, DiagnosticCode.FreeVariable, DiagnosticCode.InvalidRole, DiagnosticCode.TypeMismatch,
    DiagnosticCode.ConflictingDeclaration, DiagnosticCode.TypeErrorsInInclude, DiagnosticCode.WrongTypeArguments,
    DiagnosticCode.UndeclaredType, DiagnosticCode.UndeclaredSymbol, DiagnosticCode.MalformedType, DiagnosticCode.InvalidLetDefinition)

  /** Root text replaced in memory; included files come from disk. Each new text gets a new version stamp. */
  private final class MutantSources(root: Path) extends SourceProvider {
    private var current: Option[(String, Stamp)] = None
    private var version = 0
    def set(text: String): Unit = { version += 1; current = Some((text, Stamp.Version(version))) }
    def stamp(path: Path): Option[Stamp] = if (path == root) current.map(_._2).orElse(DiskSources.stamp(path)) else DiskSources.stamp(path)
    def read(path: Path): Option[(String, Stamp)] = if (path == root) current.orElse(DiskSources.read(path)) else DiskSources.read(path)
  }

  /** Runs up to `perKind` mutants of every kind on `root` (a valid problem below `tptpRoot`). */
  def run(tptpRoot: Path, root: Path, seed: Long, perKind: Int): Vector[TypeOutcome] = {
    val sources = new MutantSources(root)
    val project = ProjectAnalyzer(sources, ResolutionEnv(Vector.empty, Some(tptpRoot)))
    val original = project.fileAnalysis(root).getOrElse(throw new IllegalArgumentException(s"cannot read $root"))
    val text = DiskSources.read(root).map(_._1).getOrElse(throw new IllegalArgumentException(s"cannot read $root"))
    val rnd = new Random(seed * 7919L + root.toString.hashCode)
    val mutants = TypeMutationKind.values.toVector.flatMap { kind =>
      rnd.shuffle(candidates(kind, text, original, project.includeClosure(root), rnd)).take(perKind)
    }
    val name = tptpRoot.relativize(root).toString
    mutants.map { m =>
      sources.set(m.text)
      val result =
        try Right(project.analyzeRoot(root).toVector.flatMap(_.diagnostics))
        catch { case e: Throwable => Left(e) }
      result match {
        case Left(e)   => TypeOutcome(name, m, TypePrecision.Missed, Vector(s"checker threw $e"))
        case Right(ds) => grade(name, m, ds)
      }
    }
  }

  private def grade(file: String, m: TypeMutant, ds: Vector[Diagnostic]): TypeOutcome = {
    val syntax = ds.filter(_.code match { case DiagnosticCode.Syntax(_) => true; case _ => false })
    val errors = ds.filter(d => checkerErrors(d.code))
    val allowed: Set[DiagnosticCode] =
      if (m.kind.tff) Set(DiagnosticCode.TypeMismatch, DiagnosticCode.PredicateFunctionClash, DiagnosticCode.ArityMismatch,
        DiagnosticCode.TypeErrorsInInclude)
      else m.kind.expected.toSet ++
        m.kind.expected.filter(c => c != DiagnosticCode.FreeVariable).map(_ => DiagnosticCode.ConflictsInInclude) ++
        // an unbound TF1 type variable used as a type argument is also not a type
        m.kind.expected.filter(_ == DiagnosticCode.FreeVariable).map(_ => DiagnosticCode.WrongTypeArguments)
    val violations = syntax.map(d => s"syntax error '${d.message}' at ${d.span.start}") ++
      errors.filterNot(d => allowed(d.code)).map(d => s"unexpected ${d.code} '${d.message}' at ${d.span.start}")
    val precision = m.kind.expected match {
      case None => TypePrecision.Clean
      case Some(code) =>
        if (errors.exists(d => d.code == code && d.span.start == m.at)) TypePrecision.Exact
        else if (m.kind.tff && errors.exists(d => allowed(d.code) && d.span.start <= m.at && m.at < d.span.end)) TypePrecision.Exact
        else if (errors.exists(d => allowed(d.code))) TypePrecision.Displaced
        else TypePrecision.Missed
    }
    TypeOutcome(file, m, precision, violations)
  }

  private def candidates(kind: TypeMutationKind, text: String, root: FileAnalysis, closure: Vector[FileAnalysis],
                         rnd: Random): Vector[TypeMutant] = kind match {
    case TypeMutationKind.KindSwap | TypeMutationKind.AritySwap | TypeMutationKind.SameType => symbolMutants(kind, text, root, closure, rnd)
    case TypeMutationKind.Unbind | TypeMutationKind.Rebind                                  => variableMutants(kind, text, root, rnd)
    case TypeMutationKind.TffSymbolSwap | TypeMutationKind.TffSameType => tffSymbolMutants(kind, text, root, closure, rnd)
    case TypeMutationKind.NumberRetype                                 => numberMutants(text, root, closure)
    case TypeMutationKind.VariableRetype                               => variableRetypeMutants(text, root, closure)
  }

  /** The first fully known type of every symbol of the problem, with the name as first written. */
  private def signature(closure: Vector[FileAnalysis]): Map[String, (Ty, String)] = {
    val out = scala.collection.mutable.LinkedHashMap.empty[String, (Ty, String)]
    for {
      a <- closure
      case SummaryItem.Formula(_, uses) <- a.summary.items
      u <- uses if Ty.known(u.ty) && !out.contains(u.name.key)
    } out(u.name.key) = (u.ty, u.name.text)
    out.toMap
  }

  private def symbolMutants(kind: TypeMutationKind, text: String, root: FileAnalysis, closure: Vector[FileAnalysis],
                            rnd: Random): Vector[TypeMutant] = {
    val sig = signature(closure)
    def fits(t: Ty, other: Ty): Boolean = kind match {
      case TypeMutationKind.KindSwap  => Ty.result(t) != Ty.result(other)
      case TypeMutationKind.AritySwap => Ty.result(t) == Ty.result(other) && Ty.arity(t) != Ty.arity(other)
      case _                          => Ty.result(t) == Ty.result(other) && Ty.arity(t) == Ty.arity(other)
    }
    for {
      case SummaryItem.Formula(_, uses) <- root.summary.items
      u <- uses if Ty.known(u.ty)
      options = sig.toVector.filter { case (key, (t, _)) => key != u.name.key && fits(u.ty, t) }.sortBy(_._1)
      if options.nonEmpty
    } yield {
      val (_, (_, written)) = options(rnd.nextInt(options.size))
      TypeMutant(kind, s"${kind.label}: '${u.name.text}' -> '$written' at ${u.name.span.start}",
        edit(text, u.name.span, written), u.name.span.start)
    }
  }

  /** Variable uses with the names bound around them, each with its written type (TFF; `None` untyped or FOF). Uses
    * inside `$let` are left out (its definitions bind parameters). */
  private def variableUses(root: FileAnalysis): Vector[(Name, Vector[(String, Option[Ty])])] = {
    type Bound = Vector[(String, Option[Ty])]
    def formula(f: Formula, bound: Bound): Vector[(Name, Bound)] = f match {
      case a: AtomicFormula                      => a.args.toVector.flatMap(term(_, bound))
      case Equality(l, r, _, _)                  => term(l, bound) ++ term(r, bound)
      case Inequality(l, r, _, _)                => term(l, bound) ++ term(r, bound)
      case QuantifiedFormula(_, vars, body, _, _) =>
        formula(body, bound.filterNot(b => vars.exists(_.name.valid.exists(_.key == b._1))) ++
          vars.flatMap(v => v.name.valid.map(n => n.key -> v.ty.map(TffChecker.structural(_, Set.empty)))))
      case UnaryFormula(_, body, _, _)           => formula(body, bound)
      case BinaryFormula(_, l, r, _, _)          => formula(l, bound) ++ formula(r, bound)
      case VariableFormula(n, _, _)              => Vector((n, bound))
      case _: ErrorNode | _: Let | _: Unsupported => Vector.empty
    }
    def term(t: Term, bound: Bound): Vector[(Name, Bound)] = t match {
      case a: AtomicTerm     => a.args.toVector.flatMap(term(_, bound))
      case Variable(n, _)    => Vector((n, bound))
      case FormulaTerm(f, _) => formula(f, bound)
      case Tuple(es, _)      => es.toVector.flatMap(term(_, bound))
      case _                 => Vector.empty
    }
    root.tree.records.flatMap {
      case r: FofRecord => Vector(r.statement)
      case r: TffRecord if !TffChecker.unchecked(r) => Vector(r.statement)
      case _ => Vector.empty
    }.flatMap {
      case Logical(f, _)     => formula(f, Vector.empty)
      case Sequent(l, rs, _) => (l ++ rs).toVector.flatMap(formula(_, Vector.empty))
      case _                 => Vector.empty
    }
  }

  private def variableMutants(kind: TypeMutationKind, text: String, root: FileAnalysis, rnd: Random): Vector[TypeMutant] =
    variableUses(root).flatMap { case (v, bound) =>
      val replacement = kind match {
        case TypeMutationKind.Unbind => Some("ZZunbound")
        case _ =>
          // another variable bound with the same written type (both untyped in FOF)
          val own = bound.find(_._1 == v.key).flatMap(_._2)
          val others = bound.collect { case (k, t) if k != v.key && t == own => k }.distinct
          Option.when(others.nonEmpty)(others(rnd.nextInt(others.size)))
      }
      replacement.map(r => TypeMutant(kind, s"${kind.label}: '${v.text}' -> '$r' at ${v.span.start}", edit(text, v.span, r), v.span.start))
    }

  /** The closure's TFF symbols declared exactly once, not polymorphic and not types, and declared before `at` in problem
    * order (in the root before it, or in an included file when an include comes before it): key → (type, name). */
  private def tffSymbols(closure: Vector[FileAnalysis], root: FileAnalysis, at: Int = Int.MaxValue): Map[String, (Ty, String)] = {
    val includedBefore = root.tree.records.exists { case i: Include => i.span.end <= at; case _ => false }
    closure.flatMap(a => a.summary.items.collect { case SummaryItem.Declaration(_, s, ty) => (s.key, (ty, s.text, a.path == root.path, s.span.start)) })
      .groupBy(_._1).collect {
        case (k, Vector((_, (ty, written, inRoot, start))))
            if !ty.isInstanceOf[Ty.Forall] && !Ty.isType(ty) && (if (inRoot) start < at else includedBefore) =>
          k -> (ty, written)
      }
  }

  /** Nodes of the root's checked TFF formulas with their parents, outside `$let`s (whose local symbols shadow). */
  private def tffNodes(root: FileAnalysis): Vector[(Tree, Option[Tree])] = {
    def go(t: Tree, parent: Option[Tree]): Vector[(Tree, Option[Tree])] = t match {
      case _: Let => Vector.empty
      case other  => (other, parent) +: other.children.toVector.flatMap(go(_, Some(other)))
    }
    root.tree.records.collect { case r: TffRecord if !TffChecker.unchecked(r) && r.statement.isInstanceOf[Logical] => r.statement }
      .flatMap(go(_, None))
  }

  private def tffSymbolMutants(kind: TypeMutationKind, text: String, root: FileAnalysis, closure: Vector[FileAnalysis],
                               rnd: Random): Vector[TypeMutant] = {
    for {
      (node, _) <- tffNodes(root)
      (f, n) <- node match {
        case a: AtomicTerm if a.kind == AtomKind.Plain    => Some((a.f, a.args.size))
        case a: AtomicFormula if a.kind == AtomKind.Plain => Some((a.f, a.args.size))
        case _                                            => None
      }
      sig = tffSymbols(closure, root, f.span.start)
      (ty, _) <- sig.get(f.key)
      options = sig.toVector.filter { case (k, (t, _)) =>
        k != f.key && Ty.arity(t).contains(n) &&
          (if (kind == TypeMutationKind.TffSameType) Ty.equivalent(t, ty) else !Ty.compatible(t, ty))
      }.sortBy(_._1)
      if options.nonEmpty
    } yield {
      val (_, (_, written)) = options(rnd.nextInt(options.size))
      TypeMutant(kind, s"${kind.label}: '${f.text}' -> '$written' at ${f.span.start}", edit(text, f.span, written), f.span.start)
    }
  }

  /** A number whose type is checked against something else: an argument of a declared symbol, a side of `=`/`!=` whose
    * other side is not a number, or an argument of a two-argument arithmetic word. */
  private def numberMutants(text: String, root: FileAnalysis, closure: Vector[FileAnalysis]): Vector[TypeMutant] = {
    val sig = tffSymbols(closure, root)
    def twoArgumentArithmetic(w: String, n: Int) = n == 2 && Tff.arithmetic.get(w).exists {
      case Tff.Arithmetic.Test | Tff.Arithmetic.To(_) => false
      case _                                          => true
    }
    tffNodes(root).collect {
      case (n: NumberTerm, Some(parent)) if (parent match {
          case a: AtomicTerm          => sig.contains(a.f.key) || twoArgumentArithmetic(a.f.text, a.args.size)
          case a: AtomicFormula       => sig.contains(a.f.key) || twoArgumentArithmetic(a.f.text, a.args.size)
          case Equality(l, r, _, _)   => !(l.isInstanceOf[NumberTerm] && r.isInstanceOf[NumberTerm])
          case Inequality(l, r, _, _) => !(l.isInstanceOf[NumberTerm] && r.isInstanceOf[NumberTerm])
          case _                      => false
        }) =>
        val replacement = if (n.text.exists(c => c == '/' || c == '.' || c == 'e' || c == 'E')) "1" else s"${n.text}.0"
        TypeMutant(TypeMutationKind.NumberRetype, s"number retype: '${n.text}' -> '$replacement' at ${n.span.start}",
          edit(text, n.span, replacement), n.span.start)
    }
  }

  /** A typed binding `X: τ` whose variable is a direct argument of a declared symbol in its body: `τ` replaced by
    * `$int`, or `$real` if `τ` is `$int`. */
  private def variableRetypeMutants(text: String, root: FileAnalysis, closure: Vector[FileAnalysis]): Vector[TypeMutant] = {
    val sig = tffSymbols(closure, root)
    def direct(key: String, args: Seq[Term]) = args.exists { case Variable(n, _) => n.key == key; case _ => false }
    def usedAsDeclaredArgument(key: String, body: Tree): Boolean = body match {
      case a: AtomicTerm if sig.contains(a.f.key) && direct(key, a.args)    => true
      case a: AtomicFormula if sig.contains(a.f.key) && direct(key, a.args) => true
      case _: Let => false
      case other  => other.children.exists(usedAsDeclaredArgument(key, _))
    }
    tffNodes(root).collect { case (q: QuantifiedFormula, _) => q }.flatMap { q =>
      q.vars.collect {
        case VarBinding(n: Name, Some(t @ TypeApp(f, Seq(), _)), _) if f.text != "$tType" && usedAsDeclaredArgument(n.key, q.body) =>
          val replacement = if (f.text == "$int") "$real" else "$int"
          TypeMutant(TypeMutationKind.VariableRetype, s"variable retype: '${n.text}: ${f.text}' -> '$replacement' at ${t.span.start}",
            edit(text, t.span, replacement), t.span.start)
      }
    }
  }

  /** Replaces the code-point span `s` of `text`. */
  private def edit(text: String, s: Span, replacement: String): String = {
    val from = text.offsetByCodePoints(0, s.start)
    val to = text.offsetByCodePoints(0, s.end)
    text.substring(0, from) + replacement + text.substring(to)
  }

  /** A table of precision per kind, like the syntax mutation report. */
  def table(outcomes: Vector[TypeOutcome]): String = {
    val rows = TypeMutationKind.values.toVector.map { kind =>
      val os = outcomes.filter(_.mutant.kind == kind)
      def pct(p: TypePrecision) = if (os.isEmpty) 0.0 else 100.0 * os.count(_.precision == p) / os.size
      f"| ${kind.label}%-26s | ${os.size}%7d | ${pct(TypePrecision.Exact)}%6.1f%% | ${pct(TypePrecision.Displaced)}%8.1f%% | ${pct(TypePrecision.Missed)}%6.1f%% | ${os.count(_.violations.nonEmpty)}%10d |"
    }
    ("| mutation                   | mutants |   exact | displaced | missed | violations |" +:
      "|----------------------------|--------:|--------:|----------:|-------:|-----------:|" +: rows).mkString("\n")
  }
}
