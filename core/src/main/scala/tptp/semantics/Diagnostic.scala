package tptp.semantics

import java.nio.file.Path
import tptp.syntax.{ErrorCause, LineIndex, Span}

enum Severity {
  case Error, Warning, Information
}

enum DiagnosticCode {
  case Syntax(cause: ErrorCause)
  case IncludeNotFound
  case IncludeCycle
  case SelectedNameNotFound
  case ErrorsInInclude
}

/** A span in a specific file, with that file's line index for position conversion. */
final case class Location(path: Path, span: Span, lines: LineIndex)

final case class RelatedInfo(location: Location, message: String)

/** A problem in the file whose analysis holds this diagnostic. */
final case class Diagnostic(
    span: Span,
    severity: Severity,
    code: DiagnosticCode,
    message: String,
    related: Vector[RelatedInfo] = Vector.empty,
)
