package tptp.check

import java.nio.file.{Files, Path}
import tptp.analysis.{DiskSources, ProjectAnalyzer, SourceProvider, Stamp}
import tptp.semantics.*
import tptp.syntax.Span

class ProblemCheckerSuite extends munit.FunSuite {
  private val tmp = FunFixture[Path](_ => Files.createTempDirectory("problem").toRealPath(), _ => ())

  private def write(dir: Path, name: String, text: String): Path = Files.writeString(dir.resolve(name), text)
  private def analyse(main: Path): Vector[Diagnostic] =
    ProjectAnalyzer(DiskSources, ResolutionEnv(Vector.empty, None)).analyzeRoot(main).get.diagnostics
  private def checks(ds: Vector[Diagnostic]) = ds.filter(d => Set(DiagnosticCode.ArityMismatch,
    DiagnosticCode.PredicateFunctionClash, DiagnosticCode.ConflictsInInclude, DiagnosticCode.DuplicateName)(d.code))

  tmp.test("a use in the open file that conflicts with an include's first use is reported on that use") { dir =>
    write(dir, "b.ax", "fof(b1, axiom, p(c, d)).\n")
    val main = write(dir, "main.p", "include('b.ax').\nfof(g, conjecture, p(c) | f(c)).\nfof(h, axiom, c).\n")
    val ds = checks(analyse(main))
    assertEquals(ds.map(d => (d.code, d.span, d.message)), Vector(
      (DiagnosticCode.ArityMismatch, Span(36, 37), "`p` is used with 1 argument here, but with 2 at its first use"),
      (DiagnosticCode.PredicateFunctionClash, Span(64, 65), "`c` is used as a predicate here, but as a function at its first use"),
    ))
    assertEquals(ds.head.related.map(r => (r.location.path, r.location.span)), Vector((dir.resolve("b.ax"), Span(15, 16))))
  }

  tmp.test("a conflict between sibling includes is reported on the later include, naming the symbol") { dir =>
    write(dir, "a.ax", "fof(a1, axiom, p(c)).\n")
    write(dir, "b.ax", "fof(b1, axiom, p(c, c)).\n")
    val main = write(dir, "main.p", "include('a.ax').\ninclude('b.ax').\n")
    val ds = checks(analyse(main))
    assertEquals(ds.map(d => (d.code, d.span, d.message)), Vector((DiagnosticCode.ConflictsInInclude, Span(25, 31),
      "symbols used inconsistently in the included files: `p` (2 vs 1 arguments)")))
    assertEquals(ds.head.related.size, 2)
  }

  tmp.test("a conflict coming in through a later include is reported on that include") { dir =>
    write(dir, "b.ax", "fof(b1, axiom, q(c)).\n")
    val main = write(dir, "main.p", "fof(g, axiom, q).\ninclude('b.ax').\n")
    assertEquals(checks(analyse(main)).map(_.code), Vector(DiagnosticCode.ConflictsInInclude))
  }

  tmp.test("an unselected formula does not count") { dir =>
    write(dir, "b.ax", "fof(b1, axiom, p(c)).\nfof(b2, axiom, p(c, c)).\n")
    val main = write(dir, "main.p", "include('b.ax', [b1]).\nfof(g, conjecture, p(c)).\n")
    assertEquals(checks(analyse(main)), Vector.empty)
  }

  tmp.test("a selection applies to nested includes too") { dir =>
    write(dir, "c.ax", "fof(c1, axiom, p(c, c)).\n")
    write(dir, "b.ax", "include('c.ax').\nfof(b1, axiom, p(c)).\n")
    val main = write(dir, "main.p", "include('b.ax', [b1]).\n")
    assertEquals(checks(analyse(main)), Vector.empty)
  }

  tmp.test("duplicate formula names: in the open file on the name, via an include on the include line") { dir =>
    write(dir, "b.ax", "fof(a, axiom, q).\n")
    val main = write(dir, "main.p", "fof(a, axiom, p).\nfof(a, axiom, p).\ninclude('b.ax').\n")
    val ds = checks(analyse(main))
    assertEquals(ds.map(d => (d.code, d.severity, d.span)), Vector(
      (DiagnosticCode.DuplicateName, Severity.Warning, Span(22, 23)),
      (DiagnosticCode.DuplicateName, Severity.Warning, Span(44, 50)),
    ))
  }

  tmp.test("quoted and unquoted names are the same symbol") { dir =>
    val main = write(dir, "main.p", "fof(a, axiom, p(c)).\nfof('a', axiom, 'p').\n")
    assertEquals(checks(analyse(main)).map(_.code).toSet, Set(DiagnosticCode.ArityMismatch, DiagnosticCode.DuplicateName))
  }

  tmp.test("an include cycle does not loop") { dir =>
    write(dir, "b.p", "include('a.p').\nfof(b1, axiom, p(c)).\n")
    val a = write(dir, "a.p", "include('b.p').\nfof(a1, axiom, p(c)).\n")
    assertEquals(checks(analyse(a)), Vector.empty)
  }

  tmp.test("include-line diagnostics name at most five symbols and keep related links bounded") { dir =>
    write(dir, "common.ax", (1 to 8).map(i => s"fof(n$i, axiom, p$i(c)).\n").mkString)
    write(dir, "b.ax", (1 to 8).map(i => s"fof(m$i, axiom, p$i(c, c)).\n").mkString)
    val main = write(dir, "main.p", "include('common.ax').\ninclude('common.ax').\ninclude('b.ax').\n")
    val ds = checks(analyse(main))
    val duplicates = ds.filter(_.code == DiagnosticCode.DuplicateName)
    val conflicts = ds.filter(_.code == DiagnosticCode.ConflictsInInclude)
    assertEquals(duplicates.map(_.message), Vector(
      "formula names used again in the included files: `n1`, `n2`, `n3`, `n4`, `n5`, and 3 more"))
    assert(conflicts.head.message.endsWith(", and 3 more"), conflicts.head.message)
    assert((duplicates ++ conflicts).forall(_.related.size <= 10), (duplicates ++ conflicts).map(_.related.size))
  }

  tmp.test("an edit to the open file reuses the folded includes") { dir =>
    write(dir, "b.ax", "fof(b1, axiom, p(c)).\n")
    val main = dir.resolve("main.p")
    object sources extends SourceProvider {
      var text = "include('b.ax').\nfof(g, axiom, p(c)).\n"
      var version = 0
      def stamp(p: Path): Option[Stamp] = if (p == main) Some(Stamp.Version(version)) else DiskSources.stamp(p)
      def read(p: Path): Option[(String, Stamp)] = if (p == main) Some((text, Stamp.Version(version))) else DiskSources.read(p)
    }
    val project = ProjectAnalyzer(sources, ResolutionEnv(Vector.empty, None))
    assertEquals(checks(project.analyzeRoot(main).get.diagnostics), Vector.empty)
    sources.text = "include('b.ax').\nfof(g, axiom, p(c, c)).\n"
    sources.version = 1
    val ds = project.analyzeRoot(main).get.diagnostics
    assertEquals(checks(ds).map(_.code), Vector(DiagnosticCode.ArityMismatch))
    assertEquals(project.includeFoldsComputed, 1)
    assertEquals(project.includesFoldsComputed, 1)
  }

  tmp.test("a use between two includes conflicts with the earlier one, and the later include with it") { dir =>
    write(dir, "a.ax", "fof(a1, axiom, p(c)).\n")
    write(dir, "b.ax", "fof(b1, axiom, p(c, c)).\n")
    val main = write(dir, "main.p", "include('a.ax').\nfof(g, axiom, p(c, c)).\ninclude('b.ax').\n")
    assertEquals(checks(analyse(main)).map(d => (d.code, d.span)), Vector(
      (DiagnosticCode.ArityMismatch, Span(31, 32)),
      (DiagnosticCode.ConflictsInInclude, Span(49, 55)),
    ))
  }

  /** Texts set in memory over the files on disk (includes are resolved on disk), each change a new version stamp. */
  private final class MemorySources extends SourceProvider {
    private val texts = scala.collection.mutable.Map.empty[Path, (String, Stamp)]
    private var version = 0
    def set(path: Path, text: String): Path = { version += 1; texts(path) = (text, Stamp.Version(version)); path }
    def stamp(path: Path): Option[Stamp] = texts.get(path).map(_._2).orElse(DiskSources.stamp(path))
    def read(path: Path): Option[(String, Stamp)] = texts.get(path).orElse(DiskSources.read(path))
  }
  private def typeChecks(ds: Vector[Diagnostic]) = ds.filter(d => Set(DiagnosticCode.TypeMismatch, DiagnosticCode.ArityMismatch,
    DiagnosticCode.PredicateFunctionClash, DiagnosticCode.ConflictingDeclaration, DiagnosticCode.TypeErrorsInInclude,
    DiagnosticCode.UndeclaredType, DiagnosticCode.UndeclaredSymbol)(d.code))

  private val natAx = "tff(nat_type, type, nat: $tType).\ntff(zero_type, type, zero: nat).\ntff(even_type, type, even: nat > $o).\n"

  tmp.test("a declaration in an include types the root's uses, with a link to it") { dir =>
    write(dir, "nat.ax", natAx)
    val main = write(dir, "main.p", "include('nat.ax').\ntff(a, axiom, even(1)).\n")
    val ds = typeChecks(analyse(main))
    assertEquals(ds.map(d => (d.code, d.message)), Vector((DiagnosticCode.TypeMismatch,
      "argument 1 of `even` has type `$int`, but `even` expects `nat`")))
    assertEquals(ds.head.related.map(_.location.path), Vector(dir.resolve("nat.ax")))
  }

  tmp.test("a type error inside an include is reported on the include line") { dir =>
    write(dir, "nat.ax", natAx + "tff(bad, axiom, even(1)).\n")
    val main = write(dir, "main.p", "include('nat.ax').\n")
    val ds = typeChecks(analyse(main))
    assertEquals(ds.map(d => (d.code, d.span, d.message)), Vector((DiagnosticCode.TypeErrorsInInclude, Span(8, 16),
      "type errors in the included files: `even` (argument 1: `$int` vs `nat`)")))
    assertEquals(ds.head.related.map(_.location.path), Vector(dir.resolve("nat.ax")))
  }

  tmp.test("problem order: an include sees the declarations before it; a root record does not see later ones") { dir =>
    write(dir, "types.ax", natAx)
    write(dir, "uses.ax", "tff(u, axiom, even(zero)).\n")
    val ok = write(dir, "ok.p", "include('types.ax').\ninclude('uses.ax').\n")
    assertEquals(typeChecks(analyse(ok)), Vector.empty)
    val early = write(dir, "early.p", "tff(a, axiom, odd(zero)).\ninclude('types.ax').\ntff(odd_type, type, odd: nat > $o).\n")
    assertEquals(typeChecks(analyse(early)).map(d => (d.code, d.message)).headOption,
      Some((DiagnosticCode.UndeclaredSymbol, "`odd` is used before its declaration")))
  }

  tmp.test("equal redeclarations and arity overloads are fine; a different type for the same count conflicts") { dir =>
    write(dir, "nat.ax", natAx)
    val ok = write(dir, "ok.p", "include('nat.ax').\ntff(e2, type, even: nat > $o).\n" +
      "tff(c1, type, color: nat > $o).\ntff(c2, type, color: (nat * nat) > $o).\ntff(a, axiom, color(zero) & color(zero, zero)).\n")
    assertEquals(typeChecks(analyse(ok)), Vector.empty)
    val bad = write(dir, "bad.p", "include('nat.ax').\ntff(e2, type, even: $int > $o).\ntff(n2, type, nat: $tType > $tType).\n" +
      "tff(z2, type, zero: $tType).\n")
    assertEquals(typeChecks(analyse(bad)).map(d => (d.code, d.message)), Vector(
      (DiagnosticCode.ConflictingDeclaration, "`even` with 1 argument is already declared as `nat > $o`"),
      (DiagnosticCode.ConflictingDeclaration, "`nat` is already declared as `$tType`"),
      (DiagnosticCode.ConflictingDeclaration, "`zero` is already declared as a symbol"),
    ))
  }

  tmp.test("a redeclaration being typed does not conflict with the declaration it repeats") { dir =>
    write(dir, "f.ax", natAx + "tff(f_type, type, f: (nat * nat) > nat).\n")
    val viaInclude = write(dir, "a.p", "include('f.ax').\ntff(f2, type, f: (nat * ) > nat).\n")
    assertEquals(typeChecks(analyse(viaInclude)), Vector.empty)
    val inRoot = write(dir, "b.p", natAx + "tff(f1, type, f: (nat * nat) > nat).\ntff(f2, type, f: (nat * ) > nat).\n")
    assertEquals(typeChecks(analyse(inRoot)), Vector.empty)
  }

  tmp.test("only includes depending on an edited declaration are rechecked") { dir =>
    val sources = MemorySources()
    write(dir, "types.ax", natAx)
    write(dir, "uses.ax", "tff(u, axiom, even(zero)).\n")
    val main = write(dir, "main.p", "")
    def text(declaration: String, formula: String) =
      s"tff(one_type, type, one: $declaration).\ninclude('types.ax').\ninclude('uses.ax').\ntff(a, axiom, $formula).\n"
    val project = ProjectAnalyzer(sources, ResolutionEnv(Vector.empty, None))
    sources.set(main, text("nat", "even(one)"))
    project.analyzeRoot(main)
    val first = project.includeChecksComputed
    sources.set(main, text("nat", "even(zero) | even(one)"))
    project.analyzeRoot(main)
    assertEquals(project.includeChecksComputed, first, "an edit of a formula reuses every include check")
    sources.set(main, text("$int", "even(one)"))
    project.analyzeRoot(main)
    assertEquals(project.includeChecksComputed, first, "`one` is used by no include")
    sources.set(main, "tff(even_type, type, even: $int > $o).\n" + text("nat", "even(one)"))
    project.analyzeRoot(main)
    assert(project.includeChecksComputed > first, "`even` before the includes is looked up by `uses.ax`")
  }

  tmp.test("an axiom file opened on its own warns about what it does not declare") { dir =>
    val ax = write(dir, "events.ax", "tff(h, type, holdsAt: (fluent * $int) > $o).\ntff(a, axiom, ! [F: fluent] : (holdsAt(F, 1) | p(F))).\n")
    val ds = typeChecks(analyse(ax))
    assertEquals(ds.map(d => (d.code, d.severity)).distinct, Vector(
      (DiagnosticCode.UndeclaredType, Severity.Warning), (DiagnosticCode.UndeclaredSymbol, Severity.Warning)))
  }
}
