package tptp.analysis

import java.nio.file.Path
import scala.collection.mutable
import tptp.semantics.*
import tptp.syntax.{Names, Token}

final case class RootAnalysis(file: FileAnalysis, diagnostics: Vector[Diagnostic])

/** Analyses files and their include closures, caching per-file results by stamp. Confine to one thread. */
final class ProjectAnalyzer(sources: SourceProvider, initialEnv: ResolutionEnv) {
  private var env = initialEnv
  private val cache = mutable.Map.empty[Path, FileAnalysis]

  def setEnv(newEnv: ResolutionEnv): Unit = {
    env = newEnv
    cache.clear()
  }

  def invalidate(path: Path): Unit = cache.remove(path)

  def fileAnalysis(path: Path): Option[FileAnalysis] =
    sources.stamp(path) match {
      case None =>
        cache.remove(path)
        None
      case Some(stamp) =>
        cache.get(path).filter(a => a.stamp == stamp && resolutionUnchanged(a)).orElse {
          sources.read(path).map { case (text, readStamp) =>
            val analysis = FileAnalyzer.analyze(path, readStamp, text, env)
            cache(path) = analysis
            analysis
          }
        }
    }

  /** `path` and every file it (transitively) includes, each once; never cancelled. */
  /** Whether every include of `a` still resolves to the same file: files may appear or disappear. */
  private def resolutionUnchanged(a: FileAnalysis): Boolean = {
    val dir = Option(a.path.getParent).getOrElse(a.path)
    a.includes.forall(ref => ref.directive.fileName.flatMap(IncludeResolver.resolve(_, dir, env)) == ref.target)
  }

  def includeClosure(path: Path): Vector[FileAnalysis] = {
    val seen = mutable.LinkedHashMap.empty[Path, FileAnalysis]
    def visit(p: Path): Unit =
      if (!seen.contains(p)) fileAnalysis(p).foreach { a =>
        seen(p) = a
        a.includes.flatMap(_.target).foreach(visit)
      }
    visit(path)
    seen.values.toVector
  }

  /** Analyses `path` and its include closure. */
  def analyzeRoot(path: Path): Option[RootAnalysis] =
    fileAnalysis(path).map { root =>
      val extra = root.includes.flatMap(includeDiagnostics(root, _))
      RootAnalysis(root, root.diagnostics ++ extra)
    }

  def dependents(path: Path): Set[Path] = {
    val reverse: Map[Path, Vector[Path]] =
      cache.values.toVector
        .flatMap(a => a.includes.flatMap(_.target).map(_ -> a.path))
        .groupMap(_._1)(_._2)
    val found = mutable.Set.empty[Path]
    def visit(p: Path): Unit = reverse.getOrElse(p, Vector.empty).foreach { d =>
      if (found.add(d)) visit(d)
    }
    visit(path)
    found.toSet - path
  }

  private def includeDiagnostics(root: FileAnalysis, ref: IncludeRef): Vector[Diagnostic] =
    (ref.target, ref.directive.fileNameToken) match {
      case (Some(target), Some(nameToken)) =>
        fileAnalysis(target) match {
          case None =>
            Vector(Diagnostic(nameToken.span, Severity.Error, DiagnosticCode.IncludeNotFound,
              s"cannot read included file '$target'"))
          case Some(_) =>
            val closure = includeClosure(target)
            findCycle(root.path, target).map(cycleDiagnostic(nameToken, _)).toVector ++
              selectionDiagnostics(ref, closure) ++
              errorsDiagnostic(nameToken, closure.filterNot(_.path == root.path)).toVector
        }
      case _ => Vector.empty
    }

  /** The first include cycle reachable from `start` when `rootPath` is already being included. */
  private def findCycle(rootPath: Path, start: Path): Option[List[Path]] = {
    val done = mutable.Set.empty[Path]
    def visit(p: Path, stack: List[Path]): Option[List[Path]] =
      if (stack.contains(p)) Some((p :: stack.takeWhile(_ != p) ::: List(p)).reverse)
      else if (done(p)) None
      else {
        val next = fileAnalysis(p).toVector.flatMap(_.includes.flatMap(_.target))
        val found = next.iterator.map(visit(_, p :: stack)).collectFirst { case Some(c) => c }
        done += p
        found
      }
    visit(start, List(rootPath))
  }

  private def cycleDiagnostic(at: Token, cycle: List[Path]): Diagnostic =
    Diagnostic(at.span, Severity.Error, DiagnosticCode.IncludeCycle,
      s"include cycle: ${cycle.map(_.getFileName.toString).mkString(" → ")}")

  private def selectionDiagnostics(ref: IncludeRef, closure: Vector[FileAnalysis]): Vector[Diagnostic] = {
    val known = closure.flatMap(_.formulaNames.keySet).toSet
    val fileName = ref.directive.fileName.getOrElse("")
    ref.directive.selection.getOrElse(Vector.empty).filterNot(t => known(Names.key(t))).map { t =>
      Diagnostic(t.span, Severity.Error, DiagnosticCode.SelectedNameNotFound,
        s"no formula named '${Names.key(t)}' in '$fileName' or the files it includes")
    }
  }

  private def errorsDiagnostic(at: Token, included: Vector[FileAnalysis]): Option[Diagnostic] = {
    val withErrors = included.filter(_.hasErrors)
    Option.when(withErrors.nonEmpty) {
      val count = withErrors.map(_.diagnostics.count(_.severity == Severity.Error)).sum
      val related = withErrors.take(10).flatMap { a =>
        a.diagnostics.find(_.severity == Severity.Error).map(d => RelatedInfo(Location(a.path, d.span, a.lines), d.message))
      }
      Diagnostic(at.span, Severity.Error, DiagnosticCode.ErrorsInInclude,
        s"included files contain $count error(s)", related)
    }
  }
}
