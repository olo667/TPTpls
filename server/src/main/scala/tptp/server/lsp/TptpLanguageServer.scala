package tptp.server.lsp

import java.nio.file.Path
import java.util.concurrent.{CompletableFuture, ConcurrentHashMap, Executors, ThreadFactory}
import java.util.concurrent.atomic.AtomicBoolean
import org.eclipse.{lsp4j as l}
import org.eclipse.lsp4j.jsonrpc.messages.Either as JEither
import org.eclipse.lsp4j.services.{LanguageClient, LanguageClientAware, LanguageServer, TextDocumentService, WorkspaceService}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal
import tptp.analysis.*
import tptp.semantics.{PathUtil, ResolutionEnv}
import tptp.server.workspace.*
import tptp.syntax.Position

final class TptpLanguageServer(env: Map[String, String] = sys.env, onExit: Int => Unit = code => sys.exit(code))
    extends LanguageServer
    with LanguageClientAware {

  private val docs = DocumentStore()
  private val results = new ConcurrentHashMap[String, RootAnalysis]()
  /** The diagnostics last sent per document, so an identical result is not published twice. */
  private val published = new ConcurrentHashMap[String, l.PublishDiagnosticsParams]()
  private val worker = Executors.newSingleThreadExecutor(daemon("tptp-analysis"))
  private val timer = Executors.newSingleThreadScheduledExecutor(daemon("tptp-debounce"))
  @volatile private var settings = Settings.default(env)
  @volatile private var workspaceRoots = Vector.empty[Path]
  @volatile private var client: Option[LanguageClient] = None
  @volatile private var shutdownRequested = false
  @volatile private var supportsConfiguration = false
  @volatile private var supportsWatchRegistration = false
  private val missingRootAnnounced = AtomicBoolean(false)
  private val scheduler = Scheduler(() => settings.debounceMs, timer, worker)
  /** Only touched on the worker thread. */
  private val project = ProjectAnalyzer(docs, ResolutionEnv(Vector.empty, None))

  private val textDocuments = TptpTextDocumentService(this)
  private val workspace = TptpWorkspaceService(this)

  override def connect(c: LanguageClient): Unit = client = Some(c)
  override def getTextDocumentService: TextDocumentService = textDocuments
  override def getWorkspaceService: WorkspaceService = workspace

  override def initialize(params: l.InitializeParams): CompletableFuture[l.InitializeResult] = {
    workspaceRoots = Option(params.getWorkspaceFolders)
      .map(_.asScala.toVector.flatMap(f => Uris.toPath(f.getUri)))
      .filter(_.nonEmpty)
      .orElse(Option(params.getRootUri).flatMap(Uris.toPath).map(Vector(_)))
      .getOrElse(Vector.empty)
    settings = Settings.merge(settings, params.getInitializationOptions)
    val ws = Option(params.getCapabilities).flatMap(c => Option(c.getWorkspace))
    supportsConfiguration = ws.flatMap(w => Option(w.getConfiguration)).exists(_.booleanValue)
    supportsWatchRegistration = ws
      .flatMap(w => Option(w.getDidChangeWatchedFiles))
      .flatMap(d => Option(d.getDynamicRegistration))
      .exists(_.booleanValue)
    applyEnv()
    CompletableFuture.completedFuture(l.InitializeResult(capabilities, l.ServerInfo("tptp-lsp", "0.0.1")))
  }

  private def capabilities: l.ServerCapabilities = {
    val sync = l.TextDocumentSyncOptions()
    sync.setOpenClose(true)
    sync.setChange(l.TextDocumentSyncKind.Full)
    sync.setSave(l.SaveOptions(false))
    val caps = l.ServerCapabilities()
    caps.setTextDocumentSync(sync)
    caps.setDocumentSymbolProvider(java.lang.Boolean.TRUE)
    caps.setDefinitionProvider(java.lang.Boolean.TRUE)
    caps.setPositionEncoding(l.PositionEncodingKind.UTF16)
    caps
  }

  override def initialized(params: l.InitializedParams): Unit = guard("initialized") {
    if (supportsWatchRegistration) registerWatcher()
    if (supportsConfiguration) fetchConfiguration() else announceMissingRoot()
  }

  override def shutdown(): CompletableFuture[Object] = {
    shutdownRequested = true
    scheduler.shutdown()
    CompletableFuture.completedFuture(null)
  }

  override def exit(): Unit = onExit(if (shutdownRequested) 0 else 1)

  // ---- notifications (called by the services) ----

  private[lsp] def opened(uri: String, version: Int, text: String): Unit = guard("didOpen") {
    docs.open(uri, version, text).foreach { d =>
      sourceChanged(d.path)
      worker.execute(() => analyze(uri)) // no debounce on open: diagnostics and outline right away
    }
  }

  private[lsp] def changed(uri: String, version: Int, text: String): Unit = guard("didChange") {
    docs.change(uri, version, text).foreach(d => sourceChanged(d.path))
  }

  private[lsp] def closed(uri: String): Unit = guard("didClose") {
    docs.close(uri).foreach { d =>
      results.remove(uri)
      published.remove(uri)
      publish(l.PublishDiagnosticsParams(uri, java.util.List.of[l.Diagnostic]()))
      sourceChanged(d.path)
    }
  }

  private[lsp] def fileChanged(uri: String, change: l.FileChangeType): Unit = guard("didChangeWatchedFiles") {
    Uris.toPath(uri).foreach(sourceChanged)
    // a created or deleted file can change how any include resolves; re-checking is cheap thanks to the cache
    if (change != l.FileChangeType.Changed) docs.all.foreach(d => schedule(d.uri))
  }

  private[lsp] def configurationChanged(json: Any): Unit = guard("didChangeConfiguration") {
    settings = Settings.merge(settings, json)
    applyEnv()
  }

  // ---- requests ----

  private[lsp] def documentSymbols(
      uri: String): CompletableFuture[java.util.List[JEither[l.SymbolInformation, l.DocumentSymbol]]] =
    CompletableFuture.supplyAsync(
      () =>
        request("documentSymbol", java.util.List.of[JEither[l.SymbolInformation, l.DocumentSymbol]]()) {
          current(uri).map(r => Convert.outline(r.file)).getOrElse(java.util.List.of())
        },
      worker,
    )

  private[lsp] def definition(
      uri: String,
      pos: l.Position): CompletableFuture[JEither[java.util.List[? <: l.Location], java.util.List[? <: l.LocationLink]]] =
    CompletableFuture.supplyAsync(
      () => {
        val locations = request("definition", java.util.List.of[l.Location]()) {
          current(uri)
            .map { r =>
              val offset = r.file.lines.offset(Position(pos.getLine, pos.getCharacter))
              Navigation.definition(r.file, offset, project).map(Convert.location).asJava
            }
            .getOrElse(java.util.List.of[l.Location]())
        }
        JEither.forLeft[java.util.List[? <: l.Location], java.util.List[? <: l.LocationLink]](locations)
      },
      worker,
    )

  // ---- analysis ----

  private def resolutionEnv: ResolutionEnv = ResolutionEnv(workspaceRoots, settings.tptpRoot.map(PathUtil.canonical))

  private def applyEnv(): Unit = {
    val e = resolutionEnv
    worker.execute(() => guard("settings")(project.setEnv(e)))
    docs.all.foreach(d => schedule(d.uri))
  }

  /** Re-analyses every open document that is `path` or (transitively) includes it. */
  private def sourceChanged(path: Path): Unit =
    worker.execute(() =>
      guard("invalidate") {
        project.invalidate(path)
        val affected = project.dependents(path) + path
        docs.all.filter(d => affected(d.path)).foreach(d => schedule(d.uri))
      })

  private def schedule(uri: String): Unit = scheduler.schedule(uri)(analyze(uri))

  private def analyze(uri: String): Unit = guard("analysis") {
    docs.get(uri).foreach { doc =>
      project.analyzeRoot(doc.path).foreach { r =>
        r.file.stamp match {
          case Stamp.Version(v) if docs.get(uri).exists(_.version == v) =>
            results.put(uri, r)
            val params = Convert.publish(uri, v, r)
            if (published.get(uri) != params) {
              published.put(uri, params)
              publish(params)
            }
          case _ => () // stale: a newer version is already scheduled
        }
      }
    }
  }

  /** The analysis of the document's current version, computing it now if needed. Worker thread only. */
  private def current(uri: String): Option[RootAnalysis] =
    docs.get(uri).flatMap { doc =>
      def fresh = Option(results.get(uri)).filter(_.file.stamp == Stamp.Version(doc.version))
      fresh.orElse { analyze(uri); fresh }
    }

  // ---- client interaction ----

  private def registerWatcher(): Unit = {
    val watcher = l.FileSystemWatcher(JEither.forLeft[String, l.RelativePattern]("**/*.{p,ax}"))
    val options = l.DidChangeWatchedFilesRegistrationOptions(java.util.List.of(watcher))
    val registration = l.Registration("tptp-watched-files", "workspace/didChangeWatchedFiles", options)
    client.foreach(_.registerCapability(l.RegistrationParams(java.util.List.of(registration))))
  }

  private def fetchConfiguration(): Unit = {
    val item = l.ConfigurationItem()
    item.setSection("tptp")
    client.foreach(
      _.configuration(l.ConfigurationParams(java.util.List.of(item))).whenComplete { (values, error) =>
        guard("configuration") {
          if (error == null && values != null && !values.isEmpty) configurationChanged(values.get(0))
          announceMissingRoot()
        }
      })
  }

  private def announceMissingRoot(): Unit =
    if (settings.tptpRoot.isEmpty && missingRootAnnounced.compareAndSet(false, true))
      client.foreach(
        _.showMessage(l.MessageParams(
          l.MessageType.Info,
          "No TPTP root configured (setting tptp.tptpRoot or environment variable TPTP); " +
            "includes are resolved against the file's directory and the workspace only.",
        )))

  private def publish(p: l.PublishDiagnosticsParams): Unit = client.foreach(_.publishDiagnostics(p))

  private def log(message: String): Unit =
    client.foreach(_.logMessage(l.MessageParams(l.MessageType.Error, message)))

  private def guard(what: String)(body: => Unit): Unit =
    try body
    catch { case NonFatal(e) => log(s"$what failed: $e") }

  private def request[A](what: String, fallback: A)(body: => A): A =
    try body
    catch { case NonFatal(e) => log(s"$what failed: $e"); fallback }

  private def daemon(name: String): ThreadFactory = (r: Runnable) => {
    val t = new Thread(r, name)
    t.setDaemon(true)
    t
  }
}
