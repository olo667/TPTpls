package tptp.analysis

import java.nio.file.Path
import scala.collection.mutable
import tptp.check.{CheckedFile, Decl, IncludeFold, IncludesFold, ProblemChecker, ProblemDeclarations, Signature, SummaryItem, TffChecker, Ty, TypeProblem}
import tptp.semantics.*
import tptp.syntax.Span
import tptp.syntax.tree.{Include, Name, TffRecord, TypeDeclaration}

final case class RootAnalysis(file: FileAnalysis, diagnostics: Vector[Diagnostic])

/** Analyses files and their include closures, caching per-file results by stamp. Confine to one thread. */
final class ProjectAnalyzer(sources: SourceProvider, initialEnv: ResolutionEnv) {
  private var env = initialEnv
  private val cache = mutable.Map.empty[Path, FileAnalysis]
  /** Folded includes by (target, selection, root), with the stamps of the files they were folded from. */
  private val folds = mutable.Map.empty[(Path, Option[Set[String]], Path), (Vector[(Path, Stamp)], IncludeFold)]
  private var foldsComputed = 0
  /** Per root: its includes (targets and selections) and their files' stamps, with the combined fold. */
  private val problemFolds = mutable.Map.empty[Path, (Vector[(Option[Path], Option[Set[String]])], Vector[(Path, Stamp)], IncludesFold)]
  private var problemFoldsComputed = 0
  /** Per (included target, root): the stamps of its closure, its formula names, and its errors summary. */
  private val closureInfo = mutable.Map.empty[(Path, Path), (Vector[(Path, Stamp)], Set[String], Option[(Int, Vector[RelatedInfo])])]

  /** How many single-include folds were computed (not reused) so far. */
  def includeFoldsComputed: Int = foldsComputed

  /** How many combined include folds (one per problem) were computed (not reused) so far. */
  def includesFoldsComputed: Int = problemFoldsComputed

  def setEnv(newEnv: ResolutionEnv): Unit = {
    env = newEnv
    cache.clear()
    folds.clear()
    problemFolds.clear()
    closureInfo.clear()
    includeChecks.clear()
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
    a.includes.forall(ref => ref.include.fileName.valid.flatMap(n => IncludeResolver.resolve(n.key, dir, env)) == ref.target)
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
      val folds = includesFold(root)
      val decls = ProblemChecker.declarations(root.path, root.lines, root.tree.records, folds)
      val axiomFile = root.path.getFileName.toString.endsWith(".ax")
      val locate = (s: Span) => Location(root.path, s, root.lines)
      var before = 0
      val rootTypes = root.tree.records.zipWithIndex.flatMap {
        case (_: Include, _)   => before += 1; Vector.empty
        case (r: TffRecord, i) => TffChecker.typed(r, decls.at((before, 0, i)), axiomFile, locate).map(_.diagnostic)
        case _                 => Vector.empty
      }
      val extra = root.includes.flatMap(includeDiagnostics(root, _)) ++
        ProblemChecker.check(checked(root), folds) ++ ProblemChecker.declarationDiagnostics(decls) ++
        typeErrorsInIncludes(root, decls)
      RootAnalysis(root, root.diagnostics ++ extra ++ rootTypes)
    }

  /** Per (target, selection, root): the stamps of the include's files, the outside declarations its check looked up
    * (key → entries), and its type problems with their locations. */
  private val includeChecks = mutable.Map.empty[(Path, Option[Set[String]], Path),
    (Vector[(Path, Stamp)], Map[String, Vector[DeclKey]], Vector[(TypeProblem, Location)])]
  private var includeChecksCount = 0

  /** A declaration as a cache key: `Location` holds a `LineIndex`, which a re-analysed root replaces. */
  private type DeclKey = (Name, Ty, Path, Span)
  private def keyed(ds: Vector[Decl]): Vector[DeclKey] = ds.map(d => (d.symbol, d.ty, d.at.path, d.at.span))

  /** How many include type checks were computed (not reused) so far. */
  def includeChecksComputed: Int = includeChecksCount

  /** One `TypeErrorsInInclude` per include line whose files have type errors or conflicting declarations. */
  private def typeErrorsInIncludes(root: FileAnalysis, decls: ProblemDeclarations): Vector[Diagnostic] = {
    val incs = root.summary.items.collect { case SummaryItem.IncludeAt(i) => i }
    val conflicts = decls.includeConflicts
    incs.zipWithIndex.flatMap { case (inc, j) =>
      val problems = includeTypeProblems(root, decls, inc, j) ++ conflicts.getOrElse(j, Vector.empty)
      Option.when(problems.nonEmpty)(ProblemChecker.typeErrorsInInclude(inc, problems))
    }
  }

  /** The type problems of include `j`'s selected records, each checked against the declarations before it; reused
    * while the include's files are unchanged and the outside declarations it looked up are the same. */
  private def includeTypeProblems(root: FileAnalysis, decls: ProblemDeclarations, inc: Include, j: Int): Vector[(TypeProblem, Location)] =
    root.includes.find(_.include eq inc).flatMap(_.target).toVector.flatMap { target =>
      val selection = ProblemChecker.keys(inc)
      val key = (target, selection, root.path)
      val stamps = stampsOf(Vector(target), root.path)
      val outside = decls.outside(j)
      includeChecks.get(key).collect {
        case (s, deps, ps) if s == stamps && deps.forall { case (k, ds) => keyed(outside(k)) == ds } => ps
      }.getOrElse {
        val looked = mutable.Map.empty[String, Vector[DeclKey]]
        var seq = 0 // own selected declarations before the current record, in fold order
        val ps = selectedTffRecords(target, selection, root.path).flatMap { case (a, r) =>
          val view = decls.at((j, 1, seq))
          val recording = new Signature {
            def apply(k: String): Vector[Decl] = { looked.getOrElseUpdate(k, keyed(outside(k))); view(k) }
            def later(k: String): Option[Decl] = view.later(k)
          }
          val found = TffChecker.typed(r, recording, axiomFile = false, s => Location(a.path, s, a.lines))
            .map(p => (p, Location(a.path, p.diagnostic.span, a.lines)))
          r.statement match {
            case TypeDeclaration(s, _, _) if s.valid.isDefined => seq += 1
            case _                                            => ()
          }
          found
        }
        includeChecksCount += 1
        includeChecks(key) = (stamps, looked.toMap, ps)
        ps
      }
    }

  /** The TFF records of `target`'s closure that the selection keeps, in fold order, with their files. */
  private def selectedTffRecords(target: Path, selection: Option[Set[String]], root: Path): Vector[(FileAnalysis, TffRecord)] = {
    val out = Vector.newBuilder[(FileAnalysis, TffRecord)]
    def visit(p: Path, sel: Option[Set[String]], stack: List[Path]): Unit = fileAnalysis(p).foreach { a =>
      a.tree.records.foreach {
        case r: TffRecord if sel.forall(s => r.name.valid.exists(n => s(n.key))) => out += ((a, r))
        case i: Include =>
          a.includes.find(_.include eq i).flatMap(_.target).filterNot(stack.contains).foreach { t =>
            visit(t, ProblemChecker.combineSelections(sel, ProblemChecker.keys(i)), t :: stack)
          }
        case _ => ()
      }
    }
    visit(target, selection, List(target, root))
    out.result()
  }

  /** All of `root`'s includes folded together, reused while its includes and their files are unchanged. */
  private def includesFold(root: FileAnalysis): IncludesFold = {
    val incs = root.summary.items.collect { case SummaryItem.IncludeAt(i) => i }
    val key = incs.map(i => (root.includes.find(_.include eq i).flatMap(_.target), ProblemChecker.keys(i)))
    val stamps = stampsOf(key.flatMap(_._1).distinct, root.path)
    problemFolds.get(root.path).collect { case (k, s, f) if k == key && s == stamps => f }.getOrElse {
      val f = ProblemChecker.combine(incs.map(includeFold(root)))
      problemFoldsComputed += 1
      problemFolds(root.path) = (key, stamps, f)
      f
    }
  }

  /** The stamps of the files the `targets` include, transitively, other than `root`. */
  private def stampsOf(targets: Vector[Path], root: Path): Vector[(Path, Stamp)] =
    targets.flatMap(includeClosure).distinctBy(_.path).filterNot(_.path == root).map(a => a.path -> a.stamp)

  /** The fold of one of `root`'s includes, reused while the stamps of the files it covers are unchanged. */
  private def includeFold(root: FileAnalysis)(inc: Include): Option[IncludeFold] =
    root.includes.find(_.include eq inc).flatMap(_.target).flatMap { target =>
      val selection = ProblemChecker.keys(inc)
      val stamps = stampsOf(Vector(target), root.path)
      val key = (target, selection, root.path)
      folds.get(key).collect { case (s, f) if s == stamps => f }.orElse {
        fileAnalysis(target).map { t =>
          val f = ProblemChecker.fold(checked(t), selection, root.path, p => fileAnalysis(p).map(checked))
          foldsComputed += 1
          folds(key) = (stamps, f)
          f
        }
      }
    }

  private def checked(a: FileAnalysis): CheckedFile =
    CheckedFile(a.path, a.lines, a.summary, a.includes.flatMap(r => r.target.map(r.include -> _)).toMap)

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
    (ref.target, ref.include.fileName.valid) match {
      case (Some(target), Some(name)) =>
        fileAnalysis(target) match {
          case None =>
            Vector(Diagnostic(name.span, Severity.Error, DiagnosticCode.IncludeNotFound,
              s"cannot read included file '$target'"))
          case Some(_) =>
            val (known, errors) = closureNamesAndErrors(target, root.path)
            findCycle(root.path, target).map(cycleDiagnostic(name.span, _)).toVector ++
              selectionDiagnostics(ref, known) ++
              errors.map { case (count, related) =>
                Diagnostic(name.span, Severity.Error, DiagnosticCode.ErrorsInInclude, s"included files contain $count error(s)", related)
              }.toVector
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

  private def cycleDiagnostic(at: Span, cycle: List[Path]): Diagnostic =
    Diagnostic(at, Severity.Error, DiagnosticCode.IncludeCycle,
      s"include cycle: ${cycle.map(_.getFileName.toString).mkString(" → ")}")

  /** The formula names of `target`'s closure, and the error count and first errors of its files other than `root`;
    * reused while the closure's files are unchanged. */
  private def closureNamesAndErrors(target: Path, root: Path): (Set[String], Option[(Int, Vector[RelatedInfo])]) = {
    val closure = includeClosure(target)
    val stamps = closure.map(a => a.path -> a.stamp)
    closureInfo.get((target, root)).collect { case (s, known, errors) if s == stamps => (known, errors) }.getOrElse {
      val known = closure.flatMap(_.formulaNames.keySet).toSet
      val withErrors = closure.filterNot(_.path == root).filter(_.hasErrors)
      val errors = Option.when(withErrors.nonEmpty) {
        val count = withErrors.map(_.diagnostics.count(_.severity == Severity.Error)).sum
        val related = withErrors.take(10).flatMap { a =>
          a.diagnostics.find(_.severity == Severity.Error).map(d => RelatedInfo(Location(a.path, d.span, a.lines), d.message))
        }
        (count, related)
      }
      closureInfo((target, root)) = (stamps, known, errors)
      (known, errors)
    }
  }

  private def selectionDiagnostics(ref: IncludeRef, known: Set[String]): Vector[Diagnostic] = {
    val fileName = ref.include.fileName.valid.map(_.key).getOrElse("")
    ref.include.selection.getOrElse(Vector.empty).flatMap(_.valid).filterNot(n => known(n.key)).toVector.map { n =>
      Diagnostic(n.span, Severity.Error, DiagnosticCode.SelectedNameNotFound,
        s"no formula named '${n.key}' in '$fileName' or the files it includes")
    }
  }

}
