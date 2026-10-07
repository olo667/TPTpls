package tptp.analysis

import java.nio.file.Path
import tptp.semantics.*
import tptp.syntax.*

final case class IncludeRef(directive: IncludeDirective, target: Option[Path])

enum OutlineKind {
  case Formula, Include, Error
}

final case class OutlineItem(label: String, detail: String, kind: OutlineKind, span: Span, selection: Span)

/** Everything derived from one file's text alone (plus include resolution). */
final case class FileAnalysis(
    path: Path,
    stamp: Stamp,
    parsed: ParsedFile,
    records: Vector[Record],
    includes: Vector[IncludeRef],
    formulaNames: Map[String, Vector[Token]],
    outline: Vector[OutlineItem],
    diagnostics: Vector[Diagnostic],
) {
  def lines: LineIndex = parsed.cst.lines
  def hasErrors: Boolean = diagnostics.exists(_.severity == Severity.Error)
}

object FileAnalyzer {
  def analyze(
      path: Path,
      stamp: Stamp,
      text: String,
      env: ResolutionEnv,
  ): FileAnalysis = {
    val parsed = SyntaxParser.parse(text)
    val records = parsed.cst.records.flatMap(Record.of)
    val includingDir = Option(path.getParent).getOrElse(path)
    val includes = records.collect { case Record.Include(d) =>
      IncludeRef(d, d.fileName.flatMap(IncludeResolver.resolve(_, includingDir, env)))
    }
    val syntaxDiagnostics =
      parsed.errors.map(e => Diagnostic(e.span, Severity.Error, DiagnosticCode.Syntax(e.cause), e.message))
    val searched = IncludeResolver.searchDirs(includingDir, env).mkString(", ")
    val includeDiagnostics = for {
      ref <- includes if ref.target.isEmpty
      name <- ref.directive.fileName
      token <- ref.directive.fileNameToken
    } yield Diagnostic(
      token.span,
      Severity.Error,
      DiagnosticCode.IncludeNotFound,
      s"cannot find included file '$name' (searched: $searched)",
    )
    val formulaNames = records.collect { case Record.Formula(f) => f.name }.flatten.groupBy(Names.key)
    FileAnalysis(path, stamp, parsed, records, includes, formulaNames, outline(parsed.cst),
      syntaxDiagnostics ++ includeDiagnostics)
  }

  private def outline(cst: CstFile): Vector[OutlineItem] =
    cst.records.collect { case n: Node => n }.map { n =>
      Record.of(n) match {
        case Some(Record.Formula(f)) =>
          OutlineItem(
            f.name.map(Names.key).getOrElse("<error>"),
            (f.language +: f.role.map(_.text).toVector).mkString(", "),
            OutlineKind.Formula,
            n.span,
            f.name.orElse(f.keyword).map(_.span).getOrElse(n.span),
          )
        case Some(Record.Include(d)) =>
          OutlineItem(
            d.fileName.getOrElse("<error>"),
            "include",
            OutlineKind.Include,
            n.span,
            d.fileNameToken.orElse(d.keyword).map(_.span).getOrElse(n.span),
          )
        case None => OutlineItem("<error>", "", OutlineKind.Error, n.span, n.span)
      }
    }
}
