package tptp.analysis

import tptp.semantics.Location
import tptp.syntax.*

object Navigation {
  /** Definition locations for the element at `offset` in `file`. v0.01: include file names and selected names. */
  def definition(file: FileAnalysis, offset: Int, project: ProjectAnalyzer): Vector[Location] = {
    val path = file.parsed.cst.pathAt(offset)
    val token = path.lastOption.collect { case t: Token => t }
    (path.headOption.flatMap(Record.of), token) match {
      case (Some(Record.Include(d)), Some(t)) =>
        val target = file.includes.find(_.directive.node.span == d.node.span).flatMap(_.target)
        target.toVector.flatMap { targetPath =>
          if (d.fileNameToken.contains(t))
            Vector(Location(targetPath, Span.empty(0), project.fileAnalysis(targetPath).map(_.lines).getOrElse(LineIndex(""))))
          else if (d.selection.exists(_.contains(t))) {
            val key = Names.key(t)
            project.includeClosure(targetPath).flatMap { a =>
              a.formulaNames.getOrElse(key, Vector.empty).map(n => Location(a.path, n.span, a.lines))
            }
          } else Vector.empty
        }
      case _ => Vector.empty
    }
  }
}
