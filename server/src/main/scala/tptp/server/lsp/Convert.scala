package tptp.server.lsp

import org.eclipse.{lsp4j as l}
import org.eclipse.lsp4j.jsonrpc.messages.Either as JEither
import scala.jdk.CollectionConverters.*
import tptp.analysis.{FileAnalysis, OutlineItem, OutlineKind, RootAnalysis}
import tptp.semantics.*
import tptp.server.workspace.Uris
import tptp.syntax.{LineIndex, Position, Span}

object Convert {
  def position(p: Position): l.Position = l.Position(p.line, p.character)

  def range(lines: LineIndex, span: Span): l.Range =
    l.Range(position(lines.position(span.start)), position(lines.position(span.end)))

  def location(loc: Location): l.Location = l.Location(Uris.fromPath(loc.path), range(loc.lines, loc.span))

  def code(c: DiagnosticCode): String = c match {
    case DiagnosticCode.Syntax(_)            => "syntax"
    case DiagnosticCode.IncludeNotFound      => "include-not-found"
    case DiagnosticCode.IncludeCycle         => "include-cycle"
    case DiagnosticCode.SelectedNameNotFound => "selected-name-not-found"
    case DiagnosticCode.ErrorsInInclude      => "errors-in-include"
  }

  def severity(s: Severity): l.DiagnosticSeverity = s match {
    case Severity.Error       => l.DiagnosticSeverity.Error
    case Severity.Warning     => l.DiagnosticSeverity.Warning
    case Severity.Information => l.DiagnosticSeverity.Information
  }

  def diagnostic(lines: LineIndex, d: Diagnostic): l.Diagnostic = {
    val out = l.Diagnostic(range(lines, d.span), d.message, severity(d.severity), "tptp", code(d.code))
    if (d.related.nonEmpty)
      out.setRelatedInformation(d.related.map(r => l.DiagnosticRelatedInformation(location(r.location), r.message)).asJava)
    out
  }

  def publish(uri: String, version: Int, r: RootAnalysis): l.PublishDiagnosticsParams =
    l.PublishDiagnosticsParams(uri, r.diagnostics.map(diagnostic(r.file.lines, _)).asJava, Integer.valueOf(version))

  def outline(f: FileAnalysis): java.util.List[JEither[l.SymbolInformation, l.DocumentSymbol]] =
    f.outline.map(item => JEither.forRight[l.SymbolInformation, l.DocumentSymbol](symbol(f.lines, item))).asJava

  private def symbol(lines: LineIndex, item: OutlineItem): l.DocumentSymbol = {
    val kind = item.kind match {
      case OutlineKind.Formula => l.SymbolKind.Object
      case OutlineKind.Include => l.SymbolKind.File
      case OutlineKind.Error   => l.SymbolKind.Null
    }
    l.DocumentSymbol(item.label, kind, range(lines, item.span), range(lines, item.selection), item.detail)
  }
}
