package tptp.analysis

import tptp.semantics.Location
import tptp.syntax.{LineIndex, Span}
import tptp.syntax.tree.{Include, Name}

object Navigation {
  /** Definition locations for the element at `offset` in `file`: include file names and selected names. */
  def definition(file: FileAnalysis, offset: Int, project: ProjectAnalyzer): Vector[Location] = {
    val path = file.tree.pathAt(offset)
    (path.headOption, path.lastOption) match {
      case (Some(include: Include), Some(name: Name)) =>
        val target = file.includes.find(_.include eq include).flatMap(_.target)
        target.toVector.flatMap { targetPath =>
          if (include.fileName == name)
            Vector(Location(targetPath, Span.empty(0), project.fileAnalysis(targetPath).map(_.lines).getOrElse(LineIndex(""))))
          else if (include.selection.exists(_.contains(name)))
            project.includeClosure(targetPath).flatMap { a =>
              a.formulaNames.getOrElse(name.key, Vector.empty).map(n => Location(a.path, n.span, a.lines))
            }
          else Vector.empty
        }
      case _ => Vector.empty
    }
  }
}
