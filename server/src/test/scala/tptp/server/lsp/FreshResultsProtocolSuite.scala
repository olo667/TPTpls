package tptp.server.lsp

import java.util.concurrent.TimeUnit
import org.eclipse.{lsp4j as l}
import scala.jdk.CollectionConverters.*

/** Requests must be answered from the current document version, even before a debounced analysis ran. */
class FreshResultsProtocolSuite extends munit.FunSuite {
  // a debounce far longer than the test timeouts: only on-demand analysis can answer in time
  private val session = FunFixture[TestSession](_ => TestSession.start(TestSession.options(debounceMs = 5000)), _.shutdown())

  private def symbols(s: TestSession, u: String) =
    s.server.getTextDocumentService
      .documentSymbol(l.DocumentSymbolParams(l.TextDocumentIdentifier(u)))
      .get(3, TimeUnit.SECONDS).asScala.map(_.getRight.getName).toVector

  session.test("the outline is complete right after opening") { s =>
    val u = s.open("a.p", "fof(a, axiom, p).\nfof(b, axiom, q).\n")
    assertEquals(symbols(s, u), Vector("a", "b"))
  }

  session.test("diagnostics are published right after opening, without waiting for the debounce") { s =>
    val u = s.open("a.p", "fof(a, axiom, p q).\n")
    val p = s.awaitDiagnostics(u, timeoutMs = 2000)(_.getDiagnostics.size > 0)
    assertEquals(p.getVersion, Integer.valueOf(1))
  }

  session.test("the outline reflects an edit made just before the request") { s =>
    val u = s.open("a.p", "fof(a, axiom, p).\n")
    assertEquals(symbols(s, u), Vector("a"))
    s.change(u, 2, "fof(a, axiom, p).\nfof(c, axiom, r).\n")
    assertEquals(symbols(s, u), Vector("a", "c"))
  }

  session.test("definition uses the current text") { s =>
    val b = s.write("b.ax", "fof(b1, axiom, p).\n")
    val u = s.open("main.p", "fof(x, axiom, p).\n")
    s.change(u, 2, "include('b.ax').\n")
    val locs = s.server.getTextDocumentService
      .definition(l.DefinitionParams(l.TextDocumentIdentifier(u), l.Position(0, 10)))
      .get(3, TimeUnit.SECONDS).getLeft.asScala.toVector
    assertEquals(locs.map(_.getUri), Vector(b.toUri.toString))
  }

  test("an on-demand analysis and the scheduled one publish a version only once") {
    val s = TestSession.start(TestSession.options(debounceMs = 50))
    try {
      val u = s.open("a.p", "fof(a, axiom, p q).\n")
      symbols(s, u)
      Thread.sleep(500) // let the scheduled analysis run too
      val published = Iterator.continually(s.client.diagnostics.poll()).takeWhile(_ != null).filter(_.getUri == u).toVector
      assertEquals(published.map(_.getVersion.intValue), Vector(1))
    } finally s.shutdown()
  }
}
