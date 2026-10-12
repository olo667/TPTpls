package tptp.check

import java.nio.file.Path
import scala.collection.mutable
import tptp.semantics.*
import tptp.syntax.{LineIndex, Span}
import tptp.syntax.tree.{Include, Name}

/** What the problem-level check needs to know about a file. `targets` maps its includes to resolved files. */
final case class CheckedFile(path: Path, lines: LineIndex, summary: FileSummary, targets: Map[Include, Path])

/** What an include contributes to its problem: its closure's selected uses and formula names, in order. Folded once
  * per include and reused while none of its files change. Uses are kept once per symbol and type: a repeat of an
  * earlier type adds no new kind of conflict, only another related link. */
final case class IncludeFold(uses: Vector[(Name, Ty, Location)], names: Vector[(Name, Location)])

/** A symbol use or formula name from the includes, with the index of the root include it came through. */
final case class IncludedUse(name: Name, ty: Ty, at: Location, include: Int)
final case class IncludedName(name: Name, at: Location, include: Int)

/** A use that conflicts with the symbol's first use. */
final case class Conflict(name: Name, later: Ty, at: Location, first: Ty, firstAt: Location)

/** A formula name used again, with the first occurrence. */
final case class Duplicate(name: Name, at: Location, firstAt: Location)

/** All of a root's includes folded together, without the root's own records, as lookup tables: per symbol and per
  * formula name, the entries in problem order, and the conflicts and duplicates among the includes themselves. Cached
  * while the includes and their files are unchanged; an edit of the root then only refolds the root's own symbols. */
final case class IncludesFold(
    uses: Map[String, Vector[IncludedUse]],
    names: Map[String, Vector[IncludedName]],
    conflicts: Vector[(String, Int, Conflict)],
    duplicates: Vector[(String, Int, Duplicate)],
)

/** Folds a problem — the root file with each include replaced, in place, by the included file's selected records —
  * into one signature, and reports conflicting uses and duplicate formula names. Symbols (and names) are folded
  * independently: each one's events in problem order, its first fully known use being its signature entry. */
object ProblemChecker {
  private val MaxNamed = 5
  private val MaxRelated = 10
  private val Root = -1

  /** The contribution of `target` included with `selection`. Nested includes are followed with their selections
    * combined; `exclude` (the root) and include cycles are cut. */
  def fold(target: CheckedFile, selection: Option[Set[String]], exclude: Path, file: Path => Option[CheckedFile]): IncludeFold = {
    val uses = Vector.newBuilder[(Name, Ty, Location)]
    val seen = mutable.Set.empty[(String, Ty)]
    val names = Vector.newBuilder[(Name, Location)]
    def visit(f: CheckedFile, selection: Option[Set[String]], stack: List[Path]): Unit =
      f.summary.items.foreach {
        case SummaryItem.Formula(name, us) =>
          if (selection.forall(sel => name.exists(n => sel(n.key)))) {
            name.foreach(n => names += ((n, Location(f.path, n.span, f.lines))))
            us.foreach(u => if (seen.add((u.name.key, u.ty))) uses += ((u.name, u.ty, Location(f.path, u.name.span, f.lines))))
          }
        case SummaryItem.IncludeAt(inc) =>
          f.targets.get(inc).filterNot(stack.contains).flatMap(file).foreach { g =>
            visit(g, combine(selection, keys(inc)), g.path :: stack)
          }
      }
    visit(target, selection, List(target.path, exclude))
    IncludeFold(uses.result(), names.result())
  }

  /** The folds of a root's includes (in order; `None` for an unresolved one) combined into lookup tables. */
  def combine(folds: Vector[Option[IncludeFold]]): IncludesFold = {
    val uses = mutable.LinkedHashMap.empty[String, Vector[IncludedUse]]
    val names = mutable.LinkedHashMap.empty[String, Vector[IncludedName]]
    folds.zipWithIndex.foreach { case (f, j) =>
      f.foreach { f =>
        f.uses.foreach { case (n, ty, at) => uses(n.key) = uses.getOrElse(n.key, Vector.empty) :+ IncludedUse(n, ty, at, j) }
        f.names.foreach { case (n, at) => names(n.key) = names.getOrElse(n.key, Vector.empty) :+ IncludedName(n, at, j) }
      }
    }
    IncludesFold(uses.toMap, names.toMap,
      uses.toVector.flatMap { case (k, es) => foldSymbol(es.map(e => (e.name, e.ty, e.at, e.include))).map { case (j, c) => (k, j, c) } },
      names.toVector.flatMap { case (k, es) => foldName(es.map(e => (e.name, e.at, e.include))).map { case (j, d) => (k, j, d) } })
  }

  /** The selected names of an include; `None` for everything. */
  def keys(inc: Include): Option[Set[String]] = inc.selection.map(_.flatMap(_.valid).map(_.key).toSet)

  private def combine(outer: Option[Set[String]], own: Option[Set[String]]): Option[Set[String]] = (outer, own) match {
    case (Some(a), Some(b)) => Some(a intersect b)
    case (a, b)             => a.orElse(b)
  }

  /** One symbol's uses in problem order → the conflicting ones, each with where it came from. */
  private def foldSymbol(events: Vector[(Name, Ty, Location, Int)]): Vector[(Int, Conflict)] = {
    var first: Option[(Ty, Location)] = None
    val out = Vector.newBuilder[(Int, Conflict)]
    events.foreach { case (n, ty, at, via) =>
      first match {
        case None => first = Some((ty, at))
        case Some((f, fAt)) =>
          if (!Ty.compatible(f, ty)) out += ((via, Conflict(n, ty, at, f, fAt)))
          else if (!Ty.known(f) && Ty.known(ty)) first = Some((ty, at))
      }
    }
    out.result()
  }

  /** One formula name's occurrences in problem order → the repeated ones. */
  private def foldName(events: Vector[(Name, Location, Int)]): Vector[(Int, Duplicate)] =
    events.headOption.toVector.flatMap { case (_, firstAt, _) =>
      events.drop(1).map { case (n, at, via) => (via, Duplicate(n, at, firstAt)) }
    }

  /** Merges root events (with the number of includes before them) and include events (with their include index)
    * into problem order: root events before include `j` are those with fewer than `j + 1` includes before them. */
  private def merge[A](root: Vector[(Int, A)], included: Vector[(Int, A)]): Vector[A] = {
    val out = Vector.newBuilder[A]
    var r = 0
    var i = 0
    while (r < root.size || i < included.size) {
      if (i >= included.size || (r < root.size && root(r)._1 <= included(i)._1)) { out += root(r)._2; r += 1 }
      else { out += included(i)._2; i += 1 }
    }
    out.result()
  }

  /** Checks `root` against its includes' tables: only the root's own symbols and names are refolded. */
  def check(root: CheckedFile, includes: IncludesFold): Vector[Diagnostic] = {
    val rootIncludes = root.summary.items.collect { case SummaryItem.IncludeAt(i) => i }
    val rootUses = mutable.LinkedHashMap.empty[String, Vector[(Int, (Name, Ty, Location, Int))]]
    val rootNames = mutable.LinkedHashMap.empty[String, Vector[(Int, (Name, Location, Int))]]
    var before = 0
    root.summary.items.foreach {
      case SummaryItem.Formula(n, us) =>
        n.foreach(x => rootNames(x.key) = rootNames.getOrElse(x.key, Vector.empty) :+ ((before, (x, Location(root.path, x.span, root.lines), Root))))
        us.foreach(u => rootUses(u.name.key) = rootUses.getOrElse(u.name.key, Vector.empty) :+
          ((before, (u.name, u.ty, Location(root.path, u.name.span, root.lines), Root))))
      case SummaryItem.IncludeAt(_) => before += 1
    }
    val conflicts = rootUses.toVector.flatMap { case (k, rs) =>
      foldSymbol(merge(rs, includes.uses.getOrElse(k, Vector.empty).map(e => (e.include, (e.name, e.ty, e.at, e.include)))))
    } ++ includes.conflicts.collect { case (k, j, c) if !rootUses.contains(k) => (j, c) }
    val duplicates = rootNames.toVector.flatMap { case (k, rs) =>
      foldName(merge(rs, includes.names.getOrElse(k, Vector.empty).map(e => (e.include, (e.name, e.at, e.include)))))
    } ++ includes.duplicates.collect { case (k, j, d) if !rootNames.contains(k) => (j, d) }

    val out = Vector.newBuilder[Diagnostic]
    conflicts.collect { case (Root, c) => c }.sortBy(_.at.span.start).foreach(c => out += onUse(c))
    duplicates.collect { case (Root, d) => d }.sortBy(_.at.span.start).foreach { d =>
      out += Diagnostic(d.at.span, Severity.Warning, DiagnosticCode.DuplicateName, s"formula name `${d.name.text}` is already used",
        Vector(RelatedInfo(d.firstAt, s"first formula named `${d.name.text}`")))
    }
    rootIncludes.indices.foreach { j =>
      val cs = conflicts.collect { case (`j`, c) => c }
      if (cs.nonEmpty) out += onInclude(rootIncludes(j), cs)
    }
    rootIncludes.indices.foreach { j =>
      val ds = duplicates.collect { case (`j`, d) => d }
      if (ds.nonEmpty)
        out += Diagnostic(includeSpan(rootIncludes(j)), Severity.Warning, DiagnosticCode.DuplicateName,
          s"formula names used again in the included files: ${named(ds.map(d => s"`${d.name.text}`").distinct)}",
          ds.flatMap(d => Vector(RelatedInfo(d.at, s"`${d.name.text}` again"), RelatedInfo(d.firstAt, s"first formula named `${d.name.text}`")))
            .take(MaxRelated))
    }
    out.result()
  }

  private def onUse(c: Conflict): Diagnostic = {
    val n = c.name.text
    if (Ty.result(c.later) != Ty.result(c.first))
      Diagnostic(c.at.span, Severity.Error, DiagnosticCode.PredicateFunctionClash,
        s"`$n` is used as ${kind(c.later)} here, but as ${kind(c.first)} at its first use",
        Vector(RelatedInfo(c.firstAt, s"first use of `$n`, as ${kind(c.first)}")))
    else
      Diagnostic(c.at.span, Severity.Error, DiagnosticCode.ArityMismatch,
        s"`$n` is used with ${arguments(c.later)} here, but with ${count(c.first)} at its first use",
        Vector(RelatedInfo(c.firstAt, s"first use of `$n`, with ${arguments(c.first)}")))
  }

  private def onInclude(inc: Include, cs: Vector[Conflict]): Diagnostic = {
    val perSymbol = cs.groupBy(_.name.key).values.map(_.head).toVector.sortBy(_.at.span.start)
    val described = perSymbol.map { c =>
      val what =
        if (Ty.result(c.later) != Ty.result(c.first)) s"${noun(c.later)} vs ${noun(c.first)}"
        else s"${count(c.later)} vs ${count(c.first)} arguments"
      s"`${c.name.text}` ($what)"
    }
    Diagnostic(includeSpan(inc), Severity.Error, DiagnosticCode.ConflictsInInclude,
      s"symbols used inconsistently in the included files: ${named(described)}",
      cs.flatMap(c => Vector(RelatedInfo(c.at, s"`${c.name.text}` used here"), RelatedInfo(c.firstAt, s"first use of `${c.name.text}`")))
        .take(MaxRelated))
  }

  /** At most `MaxNamed` items, then "and N more". */
  private def named(items: Vector[String]): String =
    items.take(MaxNamed).mkString(", ") + (if (items.size > MaxNamed) s", and ${items.size - MaxNamed} more" else "")

  private def includeSpan(inc: Include): Span = inc.fileName.valid.map(_.span).getOrElse(inc.keyword)
  private def kind(t: Ty): String = if (Ty.result(t) == Ty.Bool) "a predicate" else "a function"
  private def noun(t: Ty): String = if (Ty.result(t) == Ty.Bool) "predicate" else "function"
  private def count(t: Ty): String = Ty.arity(t).fold("?")(_.toString)
  private def arguments(t: Ty): String = Ty.arity(t) match {
    case Some(1) => "1 argument"
    case Some(n) => s"$n arguments"
    case None    => "an unknown number of arguments"
  }
}
