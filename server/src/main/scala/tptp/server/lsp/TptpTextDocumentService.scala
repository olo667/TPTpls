package tptp.server.lsp

import java.util.concurrent.CompletableFuture
import org.eclipse.{lsp4j as l}
import org.eclipse.lsp4j.jsonrpc.messages.Either as JEither
import org.eclipse.lsp4j.services.TextDocumentService

final class TptpTextDocumentService(server: TptpLanguageServer) extends TextDocumentService {
  override def didOpen(p: l.DidOpenTextDocumentParams): Unit = {
    val d = p.getTextDocument
    server.opened(d.getUri, d.getVersion, d.getText)
  }

  override def didChange(p: l.DidChangeTextDocumentParams): Unit = {
    val changes = p.getContentChanges
    if (!changes.isEmpty)
      server.changed(p.getTextDocument.getUri, p.getTextDocument.getVersion, changes.get(changes.size - 1).getText)
  }

  override def didClose(p: l.DidCloseTextDocumentParams): Unit = server.closed(p.getTextDocument.getUri)

  override def didSave(p: l.DidSaveTextDocumentParams): Unit = ()

  override def documentSymbol(
      p: l.DocumentSymbolParams): CompletableFuture[java.util.List[JEither[l.SymbolInformation, l.DocumentSymbol]]] =
    server.documentSymbols(p.getTextDocument.getUri)

  override def definition(
      p: l.DefinitionParams): CompletableFuture[JEither[java.util.List[? <: l.Location], java.util.List[? <: l.LocationLink]]] =
    server.definition(p.getTextDocument.getUri, p.getPosition)
}
