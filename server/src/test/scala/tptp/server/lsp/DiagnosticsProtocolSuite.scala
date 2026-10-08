package tptp.server.lsp

import java.util.concurrent.TimeUnit
import org.eclipse.{lsp4j as l}
import scala.jdk.CollectionConverters.*

class DiagnosticsProtocolSuite extends munit.FunSuite {
  private val session = FunFixture[TestSession](_ => TestSession.start(), _.shutdown())

  session.test("initialize advertises full sync, symbols, definition and UTF-16") { s =>
    val caps = s.init.getCapabilities
    assertEquals(caps.getTextDocumentSync.getRight.getChange, l.TextDocumentSyncKind.Full)
    assertEquals(caps.getTextDocumentSync.getRight.getOpenClose, java.lang.Boolean.TRUE)
    assert(caps.getDocumentSymbolProvider.getLeft)
    assert(caps.getDefinitionProvider.getLeft)
    assertEquals(caps.getPositionEncoding, l.PositionEncodingKind.UTF16)
  }

  session.test("syntax errors are published with the document version, and cleared when fixed") { s =>
    val u = s.open("a.p", "fof(a, axiom, p q).\n")
    val broken = s.awaitDiagnostics(u)(_.getDiagnostics.size > 0)
    assertEquals(broken.getVersion, Integer.valueOf(1))
    val d = broken.getDiagnostics.get(0)
    assertEquals(d.getCode.getLeft, "syntax")
    assertEquals(d.getSource, "tptp")
    assertEquals(d.getSeverity, l.DiagnosticSeverity.Error)
    assertEquals(d.getRange.getStart, l.Position(0, 16))
    s.change(u, 2, "fof(a, axiom, p).\n")
    val fixed = s.awaitDiagnostics(u)(_.getVersion == 2)
    assertEquals(fixed.getDiagnostics.size, 0)
  }

  session.test("rapid edits never publish an older version after a newer one") { s =>
    val u = s.open("a.p", "fof(a, axiom, p).\n")
    s.awaitDiagnostics(u)(_.getVersion == 1)
    s.change(u, 2, "fof(a, axiom, p q).\n")
    s.change(u, 3, "fof(a, axiom, p).\n")
    val latest = s.awaitDiagnostics(u)(_.getVersion == 3)
    assertEquals(latest.getDiagnostics.size, 0)
    val later = Option(s.client.diagnostics.poll(300, TimeUnit.MILLISECONDS))
    assert(later.forall(p => p.getUri != u || p.getVersion >= 3), later)
  }

  session.test("CRLF and astral characters give UTF-16 positions") { s =>
    val u = s.open("a.p", "fof(a,axiom,p).\r\nfof(b,axiom,😀).\r\n")
    val p = s.awaitDiagnostics(u)(_.getDiagnostics.size > 0)
    val starts = p.getDiagnostics.asScala.map(_.getRange.getStart).toSet
    assert(starts.contains(l.Position(1, 12)), starts)
  }

  session.test("closing a document clears its diagnostics") { s =>
    val u = s.open("a.p", "fof(a, axiom, p q).\n")
    s.awaitDiagnostics(u)(_.getDiagnostics.size > 0)
    s.close(u)
    s.awaitDiagnostics(u)(_.getDiagnostics.isEmpty)
  }

  session.test("an unresolved include is reported with its code") { s =>
    val u = s.open("a.p", "include('missing.ax').\n")
    val p = s.awaitDiagnostics(u)(_.getDiagnostics.size > 0)
    assertEquals(p.getDiagnostics.asScala.map(_.getCode.getLeft).toVector, Vector("include-not-found"))
  }

  session.test("without a TPTP root the server logs it, without interrupting the user") { s =>
    val m = s.client.messages.poll(5, TimeUnit.SECONDS)
    assert(m != null && m.getType == l.MessageType.Info && m.getMessage.contains("No TPTP root"), m)
    assertEquals(Option(s.client.shown.poll(500, TimeUnit.MILLISECONDS)), None)
  }
}
