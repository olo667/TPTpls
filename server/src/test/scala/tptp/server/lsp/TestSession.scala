package tptp.server.lsp

import com.google.gson.JsonObject
import java.nio.channels.{Channels, Pipe}
import java.nio.file.{Files, Path}
import java.util.concurrent.{CompletableFuture, LinkedBlockingQueue, TimeUnit}
import org.eclipse.{lsp4j as l}
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services.{LanguageClient, LanguageServer}

final class TestClient(config: JsonObject) extends LanguageClient {
  val diagnostics = new LinkedBlockingQueue[l.PublishDiagnosticsParams]()
  val messages = new LinkedBlockingQueue[l.MessageParams]()
  /** Only the messages shown to the user (window/showMessage); `messages` has both kinds. */
  val shown = new LinkedBlockingQueue[l.MessageParams]()
  val registrations = new LinkedBlockingQueue[l.RegistrationParams]()
  override def telemetryEvent(o: Object): Unit = ()
  override def publishDiagnostics(p: l.PublishDiagnosticsParams): Unit = diagnostics.put(p)
  override def showMessage(p: l.MessageParams): Unit = { shown.put(p); messages.put(p) }
  override def logMessage(p: l.MessageParams): Unit = messages.put(p)
  override def showMessageRequest(p: l.ShowMessageRequestParams): CompletableFuture[l.MessageActionItem] =
    CompletableFuture.completedFuture(null)
  override def registerCapability(p: l.RegistrationParams): CompletableFuture[Void] = {
    registrations.put(p)
    CompletableFuture.completedFuture(null)
  }
  override def configuration(p: l.ConfigurationParams): CompletableFuture[java.util.List[Object]] =
    CompletableFuture.completedFuture(java.util.List.of[Object](config))
}

/** A real server connected to a TestClient through in-memory pipes. */
final class TestSession(val client: TestClient, val server: LanguageServer, val dir: Path, val init: l.InitializeResult) {
  def uri(name: String): String = dir.resolve(name).toUri.toString

  def write(name: String, text: String): Path = {
    val p = dir.resolve(name)
    Files.createDirectories(p.getParent)
    Files.writeString(p, text)
  }

  def open(name: String, text: String, version: Int = 1): String = {
    val u = uri(name)
    server.getTextDocumentService.didOpen(l.DidOpenTextDocumentParams(l.TextDocumentItem(u, "tptp", version, text)))
    u
  }

  def change(u: String, version: Int, text: String): Unit =
    server.getTextDocumentService.didChange(
      l.DidChangeTextDocumentParams(
        l.VersionedTextDocumentIdentifier(u, version),
        java.util.List.of(l.TextDocumentContentChangeEvent(text)),
      ))

  def close(u: String): Unit =
    server.getTextDocumentService.didClose(l.DidCloseTextDocumentParams(l.TextDocumentIdentifier(u)))

  /** The next published diagnostics for `u` satisfying `pred`; others are skipped. */
  def awaitDiagnostics(u: String, timeoutMs: Long = 5000)(
      pred: l.PublishDiagnosticsParams => Boolean = _ => true): l.PublishDiagnosticsParams = {
    val deadline = System.nanoTime + TimeUnit.MILLISECONDS.toNanos(timeoutMs)
    while (true) {
      val left = deadline - System.nanoTime
      val p = if (left <= 0) null else client.diagnostics.poll(left, TimeUnit.NANOSECONDS)
      if (p == null) throw new AssertionError(s"no matching diagnostics for $u within ${timeoutMs}ms")
      if (p.getUri == u && pred(p)) return p
    }
    throw new IllegalStateException("unreachable")
  }

  def shutdown(): Unit = {
    server.shutdown().get(5, TimeUnit.SECONDS)
    server.exit()
  }
}

object TestSession {
  def options(debounceMs: Int = 10, tptpRoot: Option[Path] = None): JsonObject = {
    val o = new JsonObject
    o.addProperty("debounceMs", debounceMs)
    tptpRoot.foreach(p => o.addProperty("tptpRoot", p.toString))
    o
  }

  def start(
      initOptions: JsonObject = options(),
      capabilities: l.ClientCapabilities = l.ClientCapabilities(),
      config: JsonObject = new JsonObject,
  ): TestSession = {
    val dir = Files.createTempDirectory("tptp-lsp-test").toRealPath()
    val client = TestClient(config)
    val server = TptpLanguageServer(env = Map.empty, onExit = _ => ())
    val toServer = Pipe.open()
    val toClient = Pipe.open()
    val serverLauncher = LSPLauncher.createServerLauncher(
      server, Channels.newInputStream(toServer.source), Channels.newOutputStream(toClient.sink))
    server.connect(serverLauncher.getRemoteProxy)
    serverLauncher.startListening()
    val clientLauncher = LSPLauncher.createClientLauncher(
      client, Channels.newInputStream(toClient.source), Channels.newOutputStream(toServer.sink))
    clientLauncher.startListening()
    val remote = clientLauncher.getRemoteProxy
    val params = l.InitializeParams()
    params.setCapabilities(capabilities)
    params.setInitializationOptions(initOptions)
    params.setWorkspaceFolders(java.util.List.of(l.WorkspaceFolder(dir.toUri.toString, "test")))
    val result = remote.initialize(params).get(10, TimeUnit.SECONDS)
    remote.initialized(l.InitializedParams())
    TestSession(client, remote, dir, result)
  }
}
