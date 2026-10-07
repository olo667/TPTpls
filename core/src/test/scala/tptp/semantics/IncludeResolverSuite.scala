package tptp.semantics

import java.nio.file.{Files, Path}

class IncludeResolverSuite extends munit.FunSuite {
  private val tmp = FunFixture[Path](_ => Files.createTempDirectory("resolver").toRealPath(), _ => ())

  private def file(p: Path, text: String = "fof(a,axiom,p).\n"): Path = {
    Files.createDirectories(p.getParent)
    Files.writeString(p, text)
  }

  tmp.test("search order: including directory, then workspace, then TPTP root") { dir =>
    val probDir = dir.resolve("prob"); val ws = dir.resolve("ws"); val root = dir.resolve("tptp")
    Files.createDirectories(probDir)
    val env = ResolutionEnv(Vector(ws), Some(root))
    file(root.resolve("Axioms/A.ax"))
    assertEquals(IncludeResolver.resolve("Axioms/A.ax", probDir, env), Some(root.resolve("Axioms/A.ax")))
    file(ws.resolve("Axioms/A.ax"))
    assertEquals(IncludeResolver.resolve("Axioms/A.ax", probDir, env), Some(ws.resolve("Axioms/A.ax")))
    file(probDir.resolve("Axioms/A.ax"))
    assertEquals(IncludeResolver.resolve("Axioms/A.ax", probDir, env), Some(probDir.resolve("Axioms/A.ax")))
  }

  tmp.test("missing files, directories and odd names resolve to None") { dir =>
    val env = ResolutionEnv(Vector.empty, None)
    Files.createDirectories(dir.resolve("adir"))
    assertEquals(IncludeResolver.resolve("nope.ax", dir, env), None)
    assertEquals(IncludeResolver.resolve("adir", dir, env), None)
    assertEquals(IncludeResolver.resolve("", dir, env), None)
    assertEquals(IncludeResolver.resolve("bad\u0000name", dir, env), None)
  }

  tmp.test("absolute names and '..' work and results are canonical") { dir =>
    val target = file(dir.resolve("x/T.ax"))
    val env = ResolutionEnv(Vector.empty, None)
    assertEquals(IncludeResolver.resolve(target.toString, dir.resolve("y"), env), Some(target))
    Files.createDirectories(dir.resolve("y"))
    assertEquals(IncludeResolver.resolve("../x/T.ax", dir.resolve("y"), env), Some(target))
    val link = dir.resolve("link.ax")
    Files.createSymbolicLink(link, target)
    assertEquals(IncludeResolver.resolve("link.ax", dir, env), Some(target))
  }

  test("searchDirs lists the search order") {
    val env = ResolutionEnv(Vector(Path.of("/ws1"), Path.of("/ws2")), Some(Path.of("/tptp")))
    assertEquals(
      IncludeResolver.searchDirs(Path.of("/p"), env),
      Vector(Path.of("/p"), Path.of("/ws1"), Path.of("/ws2"), Path.of("/tptp")),
    )
  }
}
