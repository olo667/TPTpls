package tptp.analysis

import java.nio.file.{Files, Path}
import tptp.semantics.*
import tptp.syntax.*

class FileAnalyzerSuite extends munit.FunSuite {
  private val tmp = FunFixture[Path](_ => Files.createTempDirectory("analyzer").toRealPath(), _ => ())
  private val noEnv = ResolutionEnv(Vector.empty, None)

  tmp.test("outline lists formulas, includes and broken records") { dir =>
    Files.writeString(dir.resolve("b.ax"), "fof(b1, axiom, q).\n")
    val text = "fof(a, axiom, p).\ninclude('b.ax').\nfof(X, conjecture, q).\n"
    val a = FileAnalyzer.analyze(dir.resolve("main.p"), Stamp.Version(1), text, noEnv)
    assertEquals(a.outline.map(i => (i.label, i.detail, i.kind)), Vector(
      ("a", "fof, axiom", OutlineKind.Formula),
      ("b.ax", "include", OutlineKind.Include),
      ("<error>", "fof, conjecture", OutlineKind.Formula),
    ))
    assertEquals(a.outline(0).selection, Span(4, 5))
    assertEquals(a.includes.map(_.target), Vector(Some(dir.resolve("b.ax"))))
  }

  tmp.test("syntax errors and unresolved includes become diagnostics") { dir =>
    val text = "fof(a, axiom, p q).\ninclude('missing.ax').\n"
    val a = FileAnalyzer.analyze(dir.resolve("main.p"), Stamp.Version(1), text, noEnv)
    val codes = a.diagnostics.map(_.code)
    assert(codes.exists { case DiagnosticCode.Syntax(_) => true; case _ => false }, codes)
    val notFound = a.diagnostics.filter(_.code == DiagnosticCode.IncludeNotFound)
    assertEquals(notFound.size, 1)
    assertEquals(notFound.head.span, Span(28, 40)) // the 'missing.ax' token
    assert(notFound.head.message.contains("missing.ax"))
    assert(notFound.head.message.contains(dir.toString), notFound.head.message) // lists searched directories
    assert(a.hasErrors)
  }

  test("formula names are keyed by TPTP name identity") {
    val a = FileAnalyzer.analyze(Path.of("/x/main.p"), Stamp.Version(1), "fof(a, axiom, p).\nfof('a', axiom, q).\nfof(b, axiom, r).", noEnv)
    assertEquals(a.formulaNames.view.mapValues(_.size).toMap, Map("a" -> 2, "b" -> 1))
  }

}
