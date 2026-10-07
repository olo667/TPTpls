package tptp.analysis

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import scala.collection.mutable
import tptp.semantics.*
import tptp.syntax.Span

class ProjectAnalyzerSuite extends munit.FunSuite {
  private final class CountingSources extends SourceProvider {
    val reads = mutable.Map.empty[Path, Int].withDefaultValue(0)
    def stamp(path: Path): Option[Stamp] = DiskSources.stamp(path)
    def read(path: Path): Option[(String, Stamp)] = { reads(path) += 1; DiskSources.read(path) }
  }

  private val tmp = FunFixture[Path](_ => Files.createTempDirectory("project").toRealPath(), _ => ())
  private val noEnv = ResolutionEnv(Vector.empty, None)

  private def write(dir: Path, name: String, text: String): Path = {
    val p = dir.resolve(name)
    Files.createDirectories(p.getParent)
    Files.writeString(p, text)
  }
  private def codes(r: RootAnalysis) = r.diagnostics.map(_.code)

  tmp.test("a valid include closure has no diagnostics") { dir =>
    write(dir, "b.ax", "include('c.ax').\nfof(b1, axiom, p).\n")
    write(dir, "c.ax", "fof(c1, axiom, q).\n")
    val main = write(dir, "main.p", "include('b.ax', [b1, c1]).\nfof(g, conjecture, p).\n")
    val r = ProjectAnalyzer(DiskSources, noEnv).analyzeRoot(main).get
    assertEquals(r.diagnostics, Vector.empty)
  }

  tmp.test("a selected name that no included file defines is reported on that name") { dir =>
    write(dir, "b.ax", "fof(b1, axiom, p).\n")
    val main = write(dir, "main.p", "include('b.ax', [b1, nope]).\n")
    val r = ProjectAnalyzer(DiskSources, noEnv).analyzeRoot(main).get
    assertEquals(codes(r), Vector(DiagnosticCode.SelectedNameNotFound))
    assertEquals(r.diagnostics.head.span, Span(21, 25))
    assert(r.diagnostics.head.message.contains("nope"))
  }

  tmp.test("include cycles are reported on the include line") { dir =>
    write(dir, "b.p", "include('a.p').\n")
    val a = write(dir, "a.p", "include('b.p').\n")
    val r = ProjectAnalyzer(DiskSources, noEnv).analyzeRoot(a).get
    val cycle = r.diagnostics.filter(_.code == DiagnosticCode.IncludeCycle)
    assertEquals(cycle.size, 1)
    assertEquals(cycle.head.message, "include cycle: a.p → b.p → a.p")
    assertEquals(cycle.head.span, Span(8, 13))
  }

  tmp.test("a file including itself") { dir =>
    val a = write(dir, "a.p", "include('a.p').\n")
    val r = ProjectAnalyzer(DiskSources, noEnv).analyzeRoot(a).get
    assertEquals(codes(r), Vector(DiagnosticCode.IncludeCycle))
  }

  tmp.test("errors in included files are summarised with related locations") { dir =>
    val b = write(dir, "b.ax", "fof(b1, axiom, p q).\n")
    val main = write(dir, "main.p", "include('b.ax').\n")
    val r = ProjectAnalyzer(DiskSources, noEnv).analyzeRoot(main).get
    assertEquals(codes(r), Vector(DiagnosticCode.ErrorsInInclude))
    val related = r.diagnostics.head.related
    assertEquals(related.size, 1)
    assertEquals(related.head.location.path, b)
  }

  tmp.test("included files are read once and re-read after they change") { dir =>
    val b = write(dir, "b.ax", "fof(b1, axiom, p).\n")
    val main = write(dir, "main.p", "include('b.ax').\n")
    val sources = CountingSources()
    val project = ProjectAnalyzer(sources, noEnv)
    project.analyzeRoot(main)
    project.analyzeRoot(main)
    assertEquals(sources.reads(b), 1)
    Files.writeString(b, "fof(b2, axiom, p).\n")
    Files.setLastModifiedTime(b, FileTime.fromMillis(Files.getLastModifiedTime(b).toMillis + 2000))
    project.analyzeRoot(main)
    assertEquals(sources.reads(b), 2)
    project.invalidate(b)
    project.analyzeRoot(main)
    assertEquals(sources.reads(b), 3)
  }

  tmp.test("dependents are the files that transitively include a file") { dir =>
    val c = write(dir, "c.ax", "fof(c1, axiom, q).\n")
    val b = write(dir, "b.ax", "include('c.ax').\n")
    val a = write(dir, "a.p", "include('b.ax').\n")
    val project = ProjectAnalyzer(DiskSources, noEnv)
    project.analyzeRoot(a)
    assertEquals(project.dependents(c), Set(a, b))
    assertEquals(project.dependents(a), Set.empty[Path])
  }

  tmp.test("setEnv clears the cache so includes are resolved again") { dir =>
    val root = dir.resolve("tptp")
    write(root, "Axioms/X.ax", "fof(x, axiom, p).\n")
    val main = write(dir, "prob/main.p", "include('Axioms/X.ax').\n")
    val project = ProjectAnalyzer(DiskSources, noEnv)
    assertEquals(codes(project.analyzeRoot(main).get), Vector(DiagnosticCode.IncludeNotFound))
    project.setEnv(ResolutionEnv(Vector.empty, Some(root)))
    assertEquals(project.analyzeRoot(main).get.diagnostics, Vector.empty)
  }

  tmp.test("a missing include is resolved once the file appears") { dir =>
    val main = write(dir, "main.p", "include('b.ax').\n")
    val project = ProjectAnalyzer(DiskSources, noEnv)
    assertEquals(codes(project.analyzeRoot(main).get), Vector(DiagnosticCode.IncludeNotFound))
    write(dir, "b.ax", "fof(b1, axiom, p).\n")
    assertEquals(project.analyzeRoot(main).get.diagnostics, Vector.empty)
  }

  tmp.test("an include switches to a file that appears earlier in the search order") { dir =>
    val ws = dir.resolve("ws")
    val inWorkspace = write(ws, "b.ax", "fof(b1, axiom, p).\n")
    val main = write(dir, "prob/main.p", "include('b.ax').\n")
    val project = ProjectAnalyzer(DiskSources, ResolutionEnv(Vector(ws), None))
    assertEquals(project.analyzeRoot(main).get.file.includes.map(_.target), Vector(Some(inWorkspace)))
    val local = write(dir, "prob/b.ax", "fof(b2, axiom, q).\n")
    assertEquals(project.analyzeRoot(main).get.file.includes.map(_.target), Vector(Some(local)))
  }

  test("unreadable roots give None") {
    assertEquals(ProjectAnalyzer(DiskSources, noEnv).analyzeRoot(Path.of("/does/not/exist.p")), None)
  }
}
