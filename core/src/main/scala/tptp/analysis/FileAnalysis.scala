package tptp.analysis

import java.nio.file.Path
import tptp.semantics.*
import tptp.syntax.{LineIndex, Span, SyntaxParser}
import tptp.syntax.tree.*

final case class IncludeRef(include: Include, target: Option[Path])

enum OutlineKind {
  case Formula, Include, Error
}

final case class OutlineItem(label: String, detail: String, kind: OutlineKind, span: Span, selection: Span)

/** Everything derived from one file's text alone (plus include resolution). */
final case class FileAnalysis(
    path: Path,
    stamp: Stamp,
    tree: TptpFile,
    includes: Vector[IncludeRef],
    formulaNames: Map[String, Vector[Name]],
    outline: Vector[OutlineItem],
    diagnostics: Vector[Diagnostic],
) {
  def lines: LineIndex = tree.lines
  def hasErrors: Boolean = diagnostics.exists(_.severity == Severity.Error)
}

object FileAnalyzer {
  def analyze(
      path: Path,
      stamp: Stamp,
      text: String,
      env: ResolutionEnv,
  ): FileAnalysis = {
    val tree = SyntaxParser.parse(text)
    val includingDir = Option(path.getParent).getOrElse(path)
    val includes = tree.records.collect { case i: Include =>
      IncludeRef(i, i.fileName.valid.flatMap(n => IncludeResolver.resolve(n.key, includingDir, env)))
    }
    val syntaxDiagnostics =
      tree.allErrors.map(e => Diagnostic(e.span, Severity.Error, DiagnosticCode.Syntax(e.cause), e.message))
    val searched = IncludeResolver.searchDirs(includingDir, env).mkString(", ")
    val includeDiagnostics = for {
      ref <- includes if ref.target.isEmpty
      name <- ref.include.fileName.valid
    } yield Diagnostic(
      name.span,
      Severity.Error,
      DiagnosticCode.IncludeNotFound,
      s"cannot find included file '${name.key}' (searched: $searched)",
    )
    val formulaNames = tree.records.flatMap {
      case r: FofRecord   => r.name.valid
      case r: OtherRecord => r.name.valid
      case _              => None
    }.groupBy(_.key)
    FileAnalysis(path, stamp, tree, includes, formulaNames, outline(tree.records), syntaxDiagnostics ++ includeDiagnostics)
  }

  private def outline(records: Vector[Record]): Vector[OutlineItem] =
    records.map {
      case r: FofRecord   => formula(r, "fof", r.name, r.role)
      case r: OtherRecord => formula(r, r.language.toString.toLowerCase, r.name, r.role)
      case r: Include =>
        OutlineItem(r.fileName.valid.map(_.key).getOrElse("<error>"), "include", OutlineKind.Include, r.span,
          r.fileName.valid.map(_.span).getOrElse(r.keyword))
      case r: ErrorRecord => OutlineItem("<error>", "", OutlineKind.Error, r.span, r.span)
    }

  private def formula(r: Record, language: String, name: NameNode, role: Role): OutlineItem =
    OutlineItem(
      name.valid.map(_.key).getOrElse("<error>"),
      (language +: role.word.valid.map(_.text).toVector).mkString(", "),
      OutlineKind.Formula,
      r.span,
      name.valid.map(_.span).getOrElse(r.keyword),
    )
}
