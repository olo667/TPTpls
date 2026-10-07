package tptp.server.lsp

import org.eclipse.{lsp4j as l}
import org.eclipse.lsp4j.services.WorkspaceService
import scala.jdk.CollectionConverters.*

final class TptpWorkspaceService(server: TptpLanguageServer) extends WorkspaceService {
  override def didChangeConfiguration(p: l.DidChangeConfigurationParams): Unit =
    server.configurationChanged(p.getSettings)

  override def didChangeWatchedFiles(p: l.DidChangeWatchedFilesParams): Unit =
    p.getChanges.asScala.foreach(e => server.fileChanged(e.getUri, e.getType))
}
