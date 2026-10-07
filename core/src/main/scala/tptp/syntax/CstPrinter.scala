package tptp.syntax

/** Deterministic, line-per-element rendering of a CST, used by golden tests. */
object CstPrinter {
  def print(file: CstFile): String = {
    val sb = new StringBuilder
    file.records.foreach(render(_, 0, sb))
    sb.toString
  }

  def cause(c: ErrorCause): String = c match {
    case ErrorCause.MissingToken(e)    => s"MissingToken(${kinds(e)})"
    case ErrorCause.UnexpectedToken(f) => s"UnexpectedToken(${f.name})"
    case ErrorCause.InputMismatch(e)   => s"InputMismatch(${kinds(e)})"
    case other                         => other.toString
  }

  private def render(c: Cst, depth: Int, sb: StringBuilder): Unit = {
    sb.append("  " * depth)
    c match {
      case n: Node =>
        sb.append(s"${n.kind.name} ${fmt(n.span)}")
        n.incomplete.foreach(i => sb.append(s" !incomplete(${cause(i)})"))
        sb.append('\n')
        n.children.foreach(render(_, depth + 1, sb))
      case t: Token =>
        sb.append(s"${t.kind.name} ${quote(t.text)} ${fmt(t.span)}\n")
      case e: ErrorNode =>
        sb.append(s"!error ${cause(e.cause)} ${fmt(e.span)} ${quote(e.message)}\n")
        e.skipped.foreach(render(_, depth + 1, sb))
    }
  }

  private def kinds(ks: Set[TokenKind]): String = ks.toVector.map(_.name).sorted.mkString(", ")
  private def fmt(s: Span): String = s"${s.start}..${s.end}"
  private def quote(s: String): String =
    "\"" + s.replace("\\", "\\\\").replace("\n", "\\n").replace("\r", "\\r").replace("\"", "\\\"") + "\""
}
