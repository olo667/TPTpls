package tptp.check

import java.nio.file.Path
import scala.collection.mutable
import tptp.semantics.*
import tptp.syntax.{LineIndex, Span}
import tptp.syntax.tree.{Include, Name, Record, TffRecord, TypeDeclaration}

/** What the problem-level check needs to know about a file. `targets` maps its includes to resolved files. */
final case class CheckedFile(path: Path, lines: LineIndex, summary: FileSummary, targets: Map[Include, Path])

/** What an include contributes to its problem: its closure's selected uses and formula names, in order. Folded once
  * per include and reused while none of its files change. Uses are kept once per symbol and type: a repeat of an
  * earlier type adds no new kind of conflict, only another related link. */
final case class IncludeFold(uses: Vector[(Name, Ty, Location)], names: Vector[(Name, Location)],
                             declarations: Vector[(Name, Ty, Location)])

/** A TFF declaration from the includes: `include` is the root include it came through, `seq` its index in that
  * include's fold. */
final case class IncludedDecl(name: Name, ty: Ty, at: Location, include: Int, seq: Int)

/** A declaration that clashes with an earlier one of the same name. */
final case class DeclConflict(name: Name, ty: Ty, at: Location, first: Ty, firstAt: Location)

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
    declarations: Map[String, Vector[IncludedDecl]],
    declarationConflicts: Vector[(String, Int, DeclConflict)],
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
    val declarations = Vector.newBuilder[(Name, Ty, Location)]
    def visit(f: CheckedFile, selection: Option[Set[String]], stack: List[Path]): Unit =
      f.summary.items.foreach {
        case SummaryItem.Formula(name, us) =>
          if (selection.forall(sel => name.exists(n => sel(n.key)))) {
            name.foreach(n => names += ((n, Location(f.path, n.span, f.lines))))
            us.foreach(u => if (seen.add((u.name.key, u.ty))) uses += ((u.name, u.ty, Location(f.path, u.name.span, f.lines))))
          }
        case SummaryItem.Declaration(name, symbol, ty) =>
          if (selection.forall(sel => name.exists(n => sel(n.key)))) {
            name.foreach(n => names += ((n, Location(f.path, n.span, f.lines))))
            declarations += ((symbol, ty, Location(f.path, symbol.span, f.lines)))
          }
        case SummaryItem.IncludeAt(inc) =>
          f.targets.get(inc).filterNot(stack.contains).flatMap(file).foreach { g =>
            visit(g, combineSelections(selection, keys(inc)), g.path :: stack)
          }
      }
    visit(target, selection, List(target.path, exclude))
    IncludeFold(uses.result(), names.result(), declarations.result())
  }

  /** The folds of a root's includes (in order; `None` for an unresolved one) combined into lookup tables. */
  def combine(folds: Vector[Option[IncludeFold]]): IncludesFold = {
    val uses = mutable.LinkedHashMap.empty[String, Vector[IncludedUse]]
    val names = mutable.LinkedHashMap.empty[String, Vector[IncludedName]]
    val decls = mutable.LinkedHashMap.empty[String, Vector[IncludedDecl]]
    folds.zipWithIndex.foreach { case (f, j) =>
      f.foreach { f =>
        f.declarations.zipWithIndex.foreach { case ((n, ty, at), seq) =>
          decls(n.key) = decls.getOrElse(n.key, Vector.empty) :+ IncludedDecl(n, ty, at, j, seq)
        }
        f.uses.foreach { case (n, ty, at) => uses(n.key) = uses.getOrElse(n.key, Vector.empty) :+ IncludedUse(n, ty, at, j) }
        f.names.foreach { case (n, at) => names(n.key) = names.getOrElse(n.key, Vector.empty) :+ IncludedName(n, at, j) }
      }
    }
    IncludesFold(uses.toMap, names.toMap,
      uses.toVector.flatMap { case (k, es) => foldSymbol(es.map(e => (e.name, e.ty, e.at, e.include))).map { case (j, c) => (k, j, c) } },
      names.toVector.flatMap { case (k, es) => foldName(es.map(e => (e.name, e.at, e.include))).map { case (j, d) => (k, j, d) } },
      decls.toMap,
      decls.toVector.flatMap { case (k, es) => conflictsOf(es.map(e => (e.name, e.ty, e.at, e.include))).map { case (j, c) => (k, j, c) } })
  }

  /** The selected names of an include; `None` for everything. */
  def keys(inc: Include): Option[Set[String]] = inc.selection.map(_.flatMap(_.valid).map(_.key).toSet)

  def combineSelections(outer: Option[Set[String]], own: Option[Set[String]]): Option[Set[String]] = (outer, own) match {
    case (Some(a), Some(b)) => Some(a intersect b)
    case (a, b)             => a.orElse(b)
  }

  /** One name's declarations in problem order → those that clash with an earlier one: a symbol redeclared with the same
    * argument count and another type, a type redeclared with another type, or a name declared as a type and a symbol.
    * A declaration with a gap in its type (a syntax error) neither clashes nor counts as the first one. */
  def conflictsOf[A](events: Vector[(Name, Ty, Location, A)]): Vector[(A, DeclConflict)] = {
    val firstByArity = mutable.Map.empty[Option[Int], (Ty, Location)] // symbols, by argument count
    var firstType: Option[(Ty, Location)] = None
    var firstSymbol: Option[(Ty, Location)] = None
    def conflict(n: Name, ty: Ty, at: Location, via: A, first: (Ty, Location)) = Some((via, DeclConflict(n, ty, at, first._1, first._2)))
    events.filter { case (_, ty, _, _) => Ty.complete(ty) }.flatMap { case (n, ty, at, via) =>
      if (Ty.isType(ty))
        firstSymbol.flatMap(conflict(n, ty, at, via, _))
          .orElse(firstType.filterNot(f => Ty.equivalent(f._1, ty)).flatMap(conflict(n, ty, at, via, _)))
          .orElse { if (firstType.isEmpty) firstType = Some((ty, at)); None }
      else
        firstType.flatMap(conflict(n, ty, at, via, _))
          .orElse(firstByArity.get(Ty.arity(ty)).filterNot(f => Ty.equivalent(f._1, ty)).flatMap(conflict(n, ty, at, via, _)))
          .orElse {
            if (!firstByArity.contains(Ty.arity(ty))) firstByArity(Ty.arity(ty)) = (ty, at)
            if (firstSymbol.isEmpty) firstSymbol = Some((ty, at))
            None
          }
    }
  }

  def conflictMessage(c: DeclConflict): String =
    if (Ty.isType(c.first) && !Ty.isType(c.ty)) s"`${c.name.text}` is already declared as a type"
    else if (!Ty.isType(c.first) && Ty.isType(c.ty)) s"`${c.name.text}` is already declared as a symbol"
    else if (Ty.isType(c.ty)) s"`${c.name.text}` is already declared as `${Ty.show(c.first)}`"
    else s"`${c.name.text}` with ${Ty.arity(c.ty).fold("?")(n => if (n == 1) "1 argument" else s"$n arguments")} is already declared as `${Ty.show(c.first)}`"

  /** A place in problem order: `(j, 0, i)` for the root's record `i` with `j` includes before it, `(j, 1, s)` for
    * include `j`'s `s`-th selected declaration (and a record of it after `s` declarations). */
  type Place = (Int, Int, Int)

  /** The problem's declarations: the root's, read from its records, and the includes' from their fold. */
  def declarations(rootPath: Path, rootLines: LineIndex, rootRecords: Vector[Record], includes: IncludesFold): ProblemDeclarations = {
    var before = 0
    val own = Vector.newBuilder[(String, (Place, Decl))]
    rootRecords.zipWithIndex.foreach {
      case (_: Include, _) => before += 1
      case (TffRecord(_, _, _, TypeDeclaration(symbol, ty, _), _, _, _), i) =>
        symbol.valid.foreach(s =>
          own += s.key -> ((before, 0, i), Decl(s, TffChecker.structural(ty, Set.empty), Location(rootPath, s.span, rootLines))))
      case _ => ()
    }
    ProblemDeclarations(own.result().groupMap(_._1)(_._2), includes)
  }

  /** Conflicting declarations whose later declaration is in the root, on that declaration. */
  def declarationDiagnostics(decls: ProblemDeclarations): Vector[Diagnostic] = decls.rootConflicts.map { c =>
    Diagnostic(c.at.span, Severity.Error, DiagnosticCode.ConflictingDeclaration, conflictMessage(c),
      Vector(RelatedInfo(c.firstAt, s"first declaration of `${c.name.text}`")))
  }

  /** One include line's type errors and conflicting declarations, each with its location in the included files. */
  def typeErrorsInInclude(inc: Include, problems: Vector[(TypeProblem, Location)]): Diagnostic =
    Diagnostic(includeSpan(inc), Severity.Error, DiagnosticCode.TypeErrorsInInclude,
      s"type errors in the included files: ${named(problems.map { case (p, _) => s"`${p.subject}` (${p.brief})" }.distinct)}",
      problems.map { case (p, at) => RelatedInfo(at, p.diagnostic.message) }.take(MaxRelated))

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
      case _: SummaryItem.Declaration => ()
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

/** A problem's declarations by key, in problem order, with the view each record has of them. */
final class ProblemDeclarations(root: Map[String, Vector[(ProblemChecker.Place, Decl)]], includes: IncludesFold) {
  import ProblemChecker.Place
  private val cache = mutable.Map.empty[String, Vector[(Place, Decl)]]
  private val order = summon[Ordering[Place]]

  def entries(key: String): Vector[(Place, Decl)] = cache.getOrElseUpdate(key,
    (root.getOrElse(key, Vector.empty) ++
      includes.declarations.getOrElse(key, Vector.empty).map(e => ((e.include, 1, e.seq), Decl(e.name, e.ty, e.at))))
      .sortBy(_._1))

  /** What a record at `place` sees; `later` looks in the same part (the root, or the same include). */
  def at(place: Place): Signature = new Signature {
    def apply(key: String): Vector[Decl] = entries(key).takeWhile(e => order.lt(e._1, place)).map(_._2)
    def later(key: String): Option[Decl] = entries(key).find { case (p, _) =>
      order.lt(place, p) && p._2 == place._2 && (place._2 == 0 || p._1 == place._1)
    }.map(_._2)
  }

  /** The part of a lookup by include `j`'s records that does not come from include `j` itself. */
  def outside(j: Int)(key: String): Vector[Decl] = entries(key).takeWhile(e => order.lt(e._1, (j, 1, 0))).map(_._2)

  /** Conflicts whose later declaration is in the root. */
  def rootConflicts: Vector[DeclConflict] = root.keys.toVector.flatMap { k =>
    ProblemChecker.conflictsOf(entries(k).map { case (p, d) => (d.symbol, d.ty, d.at, p._2) }).collect { case (0, c) => c }
  }.sortBy(_.at.span.start)

  /** Conflicts whose later declaration came in through include `j`, as type problems for its include line. */
  def includeConflicts: Map[Int, Vector[(TypeProblem, Location)]] = {
    val forRootKeys = root.keys.toVector.flatMap(k =>
      ProblemChecker.conflictsOf(entries(k).map { case (p, d) => (d.symbol, d.ty, d.at, p) }).collect { case ((j, 1, _), c) => (j, c) })
    val includeOnly = includes.declarationConflicts.collect { case (k, j, c) if !root.contains(k) => (j, c) }
    (forRootKeys ++ includeOnly).groupMap(_._1) { case (_, c) =>
      (TypeProblem(c.name.text, s"declared as `${Ty.show(c.first)}` and `${Ty.show(c.ty)}`",
        Diagnostic(c.at.span, Severity.Error, DiagnosticCode.ConflictingDeclaration, ProblemChecker.conflictMessage(c))), c.at)
    }
  }
}
