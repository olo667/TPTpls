package tptp.server.lsp

import com.google.gson.JsonObject
import java.util.concurrent.TimeUnit
import org.eclipse.{lsp4j as l}
import scala.jdk.CollectionConverters.*

class FeaturesProtocolSuite extends munit.FunSuite {
  private val session = FunFixture[TestSession](_ => TestSession.start(), _.shutdown())

  private def symbols(s: TestSession, u: String) =
    s.server.getTextDocumentService
      .documentSymbol(l.DocumentSymbolParams(l.TextDocumentIdentifier(u)))
      .get(5, TimeUnit.SECONDS).asScala.map(_.getRight).toVector

  private def definition(s: TestSession, u: String, line: Int, ch: Int) =
    s.server.getTextDocumentService
      .definition(l.DefinitionParams(l.TextDocumentIdentifier(u), l.Position(line, ch)))
      .get(5, TimeUnit.SECONDS).getLeft.asScala.toVector

  session.test("document symbols list formulas, includes and broken records") { s =>
    s.write("ax/b.ax", "fof(b1, axiom, p).\n")
    val u = s.open("main.p", "fof(a, axiom, p).\ninclude('ax/b.ax').\nfof(X, conjecture, q).\n")
    s.awaitDiagnostics(u)()
    val syms = symbols(s, u)
    assertEquals(syms.map(_.getName), Vector("a", "ax/b.ax", "<error>"))
    assertEquals(syms(0).getDetail, "fof, axiom")
    assertEquals(syms(1).getKind, l.SymbolKind.File)
    assertEquals(syms(0).getSelectionRange, l.Range(l.Position(0, 4), l.Position(0, 5)))
  }

  session.test("definition on an include name opens the included file") { s =>
    val b = s.write("ax/b.ax", "fof(b1, axiom, p).\n")
    val u = s.open("main.p", "include('ax/b.ax').\n")
    s.awaitDiagnostics(u)()
    val locs = definition(s, u, 0, 10)
    assertEquals(locs.map(_.getUri), Vector(b.toUri.toString))
    assertEquals(locs.head.getRange, l.Range(l.Position(0, 0), l.Position(0, 0)))
  }

  session.test("definition on a selected name goes to that formula") { s =>
    val b = s.write("ax/b.ax", "fof(b1, axiom, p).\nfof(b2, axiom, q).\n")
    val u = s.open("main.p", "include('ax/b.ax', [b2]).\n")
    s.awaitDiagnostics(u)()
    val locs = definition(s, u, 0, 21)
    assertEquals(locs.map(_.getUri), Vector(b.toUri.toString))
    assertEquals(locs.head.getRange, l.Range(l.Position(1, 4), l.Position(1, 6)))
  }

  session.test("a watched-file change re-checks documents that include it") { s =>
    val b = s.write("ax/b.ax", "fof(b1, axiom, p).\nfof(b2, axiom, q).\n")
    val u = s.open("main.p", "include('ax/b.ax', [b2]).\n")
    s.awaitDiagnostics(u)(_.getDiagnostics.isEmpty)
    s.write("ax/b.ax", "fof(b1, axiom, p).\n")
    s.server.getWorkspaceService.didChangeWatchedFiles(
      l.DidChangeWatchedFilesParams(java.util.List.of(l.FileEvent(b.toUri.toString, l.FileChangeType.Changed))))
    val p = s.awaitDiagnostics(u)(_.getDiagnostics.size > 0)
    assertEquals(p.getDiagnostics.asScala.map(_.getCode.getLeft).toVector, Vector("selected-name-not-found"))
  }

  session.test("creating a missing included file clears include-not-found") { s =>
    val u = s.open("main.p", "include('b.ax').\n")
    s.awaitDiagnostics(u)(_.getDiagnostics.size > 0)
    val b = s.write("b.ax", "fof(b1, axiom, p).\n")
    s.server.getWorkspaceService.didChangeWatchedFiles(
      l.DidChangeWatchedFilesParams(java.util.List.of(l.FileEvent(b.toUri.toString, l.FileChangeType.Created))))
    s.awaitDiagnostics(u)(_.getDiagnostics.isEmpty)
  }

  session.test("errors in an included file are summarised on the include line") { s =>
    s.write("b.ax", "fof(b1, axiom, p q).\n")
    val u = s.open("main.p", "include('b.ax').\n")
    val p = s.awaitDiagnostics(u)(_.getDiagnostics.size > 0)
    val d = p.getDiagnostics.get(0)
    assertEquals(d.getCode.getLeft, "errors-in-include")
    assertEquals(d.getRelatedInformation.size, 1)
  }

  session.test("requests at impossible positions or for unknown documents return empty results") { s =>
    val u = s.open("main.p", "fof(a, axiom, p).\n")
    s.awaitDiagnostics(u)()
    assertEquals(definition(s, u, 999, 999), Vector.empty)
    assertEquals(definition(s, s.uri("nope.p"), 0, 0), Vector.empty)
    assertEquals(symbols(s, s.uri("nope.p")), Vector.empty)
  }

  session.test("non-file documents are ignored without breaking the server") { s =>
    s.server.getTextDocumentService.didOpen(
      l.DidOpenTextDocumentParams(l.TextDocumentItem("untitled:Untitled-1", "tptp", 1, "fof(a, axiom, p q).")))
    assertEquals(symbols(s, "untitled:Untitled-1"), Vector.empty)
    val u = s.open("main.p", "fof(a, axiom, p).\n")
    s.awaitDiagnostics(u)()
  }

  test("dynamic registration for watched files is requested when supported") {
    val caps = l.ClientCapabilities()
    val ws = l.WorkspaceClientCapabilities()
    ws.setDidChangeWatchedFiles(l.DidChangeWatchedFilesCapabilities(true))
    caps.setWorkspace(ws)
    val s = TestSession.start(capabilities = caps)
    try {
      val reg = s.client.registrations.poll(5, TimeUnit.SECONDS)
      assert(reg != null)
      assertEquals(reg.getRegistrations.get(0).getMethod, "workspace/didChangeWatchedFiles")
    } finally s.shutdown()
  }

  test("tptpRoot from workspace/configuration is used for includes") {
    val caps = l.ClientCapabilities()
    val ws = l.WorkspaceClientCapabilities()
    ws.setConfiguration(true)
    caps.setWorkspace(ws)
    val rootDir = java.nio.file.Files.createTempDirectory("tptp-root").toRealPath()
    java.nio.file.Files.createDirectories(rootDir.resolve("Axioms"))
    java.nio.file.Files.writeString(rootDir.resolve("Axioms/X.ax"), "fof(x, axiom, p).\n")
    val config = new JsonObject
    config.addProperty("tptpRoot", rootDir.toString)
    val s = TestSession.start(capabilities = caps, config = config)
    try {
      val u = s.open("prob/main.p", "include('Axioms/X.ax').\n")
      s.awaitDiagnostics(u)(_.getDiagnostics.isEmpty)
    } finally s.shutdown()
  }
}
