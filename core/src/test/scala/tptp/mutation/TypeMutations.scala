package tptp.mutation

import java.nio.file.Path
import scala.util.Random
import tptp.analysis.{DiskSources, FileAnalysis, ProjectAnalyzer, SourceProvider, Stamp}
import tptp.check.{SummaryItem, Ty}
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
}

/** A name in the root file replaced by `replacement`; `at` is where the new name starts. */
final case class TypeMutant(kind: TypeMutationKind, description: String, text: String, at: Int)

enum TypePrecision { case Exact, Displaced, Missed, Clean }

final case class TypeOutcome(file: String, mutant: TypeMutant, precision: TypePrecision, violations: Vector[String])

/** Type mutants of a valid problem: symbol and variable names swapped for others from the same problem. */
object TypeMutations {
  private val checkerErrors: Set[DiagnosticCode] = Set(DiagnosticCode.ArityMismatch, DiagnosticCode.PredicateFunctionClash,
    DiagnosticCode.ConflictsInInclude, DiagnosticCode.FreeVariable, DiagnosticCode.InvalidRole)

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
    val allowed: Set[DiagnosticCode] = m.kind.expected.toSet ++
      m.kind.expected.filter(c => c != DiagnosticCode.FreeVariable).map(_ => DiagnosticCode.ConflictsInInclude)
    val violations = syntax.map(d => s"syntax error '${d.message}' at ${d.span.start}") ++
      errors.filterNot(d => allowed(d.code)).map(d => s"unexpected ${d.code} '${d.message}' at ${d.span.start}")
    val precision = m.kind.expected match {
      case None => TypePrecision.Clean
      case Some(code) =>
        if (errors.exists(d => d.code == code && d.span.start == m.at)) TypePrecision.Exact
        else if (errors.exists(d => allowed(d.code))) TypePrecision.Displaced
        else TypePrecision.Missed
    }
    TypeOutcome(file, m, precision, violations)
  }

  private def candidates(kind: TypeMutationKind, text: String, root: FileAnalysis, closure: Vector[FileAnalysis],
                         rnd: Random): Vector[TypeMutant] = kind match {
    case TypeMutationKind.KindSwap | TypeMutationKind.AritySwap | TypeMutationKind.SameType => symbolMutants(kind, text, root, closure, rnd)
    case TypeMutationKind.Unbind | TypeMutationKind.Rebind                                  => variableMutants(kind, text, root, rnd)
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

  /** Variable uses with the names bound around them. */
  private def variableUses(root: FileAnalysis): Vector[(Name, Vector[String])] = {
    def formula(f: Formula, bound: Vector[String]): Vector[(Name, Vector[String])] = f match {
      case a: AtomicFormula                      => a.args.toVector.flatMap(term(_, bound))
      case Equality(l, r, _, _)                  => term(l, bound) ++ term(r, bound)
      case Inequality(l, r, _, _)                => term(l, bound) ++ term(r, bound)
      case QuantifiedFormula(_, vars, body, _, _) => formula(body, bound ++ vars.flatMap(_.valid).map(_.key))
      case UnaryFormula(_, body, _, _)           => formula(body, bound)
      case BinaryFormula(_, l, r, _, _)          => formula(l, bound) ++ formula(r, bound)
      case _: ErrorNode                          => Vector.empty
    }
    def term(t: Term, bound: Vector[String]): Vector[(Name, Vector[String])] = t match {
      case a: AtomicTerm   => a.args.toVector.flatMap(term(_, bound))
      case Variable(n, _)  => Vector((n, bound.distinct))
      case _               => Vector.empty
    }
    root.tree.records.collect { case r: FofRecord => r.statement }.flatMap {
      case Logical(f, _)     => formula(f, Vector.empty)
      case Sequent(l, rs, _) => (l ++ rs).toVector.flatMap(formula(_, Vector.empty))
    }
  }

  private def variableMutants(kind: TypeMutationKind, text: String, root: FileAnalysis, rnd: Random): Vector[TypeMutant] =
    variableUses(root).flatMap { case (v, bound) =>
      val replacement = kind match {
        case TypeMutationKind.Unbind => Some("ZZunbound")
        case _ =>
          val others = bound.filterNot(_ == v.key)
          Option.when(others.nonEmpty)(others(rnd.nextInt(others.size)))
      }
      replacement.map(r => TypeMutant(kind, s"${kind.label}: '${v.text}' -> '$r' at ${v.span.start}", edit(text, v.span, r), v.span.start))
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
