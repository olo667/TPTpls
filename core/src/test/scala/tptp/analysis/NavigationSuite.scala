package tptp.analysis

import java.nio.file.{Files, Path}
import tptp.semantics.*
import tptp.syntax.Span

class NavigationSuite extends munit.FunSuite {
  private val tmp = FunFixture[Path](_ => Files.createTempDirectory("nav").toRealPath(), _ => ())

  private def setup(dir: Path): (ProjectAnalyzer, FileAnalysis, Path) = {
    val b = dir.resolve("b.ax")
    Files.writeString(b, "fof(b1, axiom, p).\nfof(b2, axiom, q).\n")
    val main = dir.resolve("main.p")
    Files.writeString(main, "include('b.ax', [b2]).\nfof(g, conjecture, p).\n")
    val project = ProjectAnalyzer(DiskSources, ResolutionEnv(Vector.empty, None))
    (project, project.fileAnalysis(main).get, b)
  }

  tmp.test("include file name goes to the start of the included file") { dir =>
    val (project, main, b) = setup(dir)
    val locs = Navigation.definition(main, 10, project)
    assertEquals(locs.map(l => (l.path, l.span)), Vector((b, Span(0, 0))))
  }

  tmp.test("a selected name goes to that formula's name in the included file") { dir =>
    val (project, main, b) = setup(dir)
    val locs = Navigation.definition(main, 17, project) // inside "b2"
    assertEquals(locs.map(l => (l.path, l.span)), Vector((b, Span(23, 25))))
  }

  tmp.test("anything else has no definition") { dir =>
    val (project, main, _) = setup(dir)
    assertEquals(Navigation.definition(main, 30, project), Vector.empty)
    assertEquals(Navigation.definition(main, 9999, project), Vector.empty)
  }
}
