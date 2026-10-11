package tptp.syntax.tree

import tptp.syntax.{ErrorCause, Span, SyntaxError, TokenKind}

/** Deterministic, line-per-node rendering of the compact tree, for golden tests and mutation checks. */
object TreePrinter {
  def print(file: TptpFile): String = {
    val sb = new StringBuilder
    file.errors.foreach(e => sb.append("! ").append(error(e, 0)).append('\n'))
    file.records.foreach(render(_, 0, 0, sb))
    sb.toString
  }

  /** One record, with spans relative to `base` (the record's start, to compare records that moved). */
  def print(record: Record, base: Int): String = {
    val sb = new StringBuilder
    render(record, 0, base, sb)
    sb.toString
  }

  def cause(c: ErrorCause): String = c match {
    case ErrorCause.MissingToken(e)    => s"MissingToken(${kinds(e)})"
    case ErrorCause.UnexpectedToken(f) => s"UnexpectedToken(${f.name})"
    case ErrorCause.InputMismatch(e)   => s"InputMismatch(${kinds(e)})"
    case other                         => other.toString
  }

  private def render(t: Tree, depth: Int, base: Int, sb: StringBuilder): Unit = {
    sb.append("  " * depth).append(header(t, base)).append('\n')
    t match {
      case r: Record => r.errors.foreach(e => sb.append("  " * (depth + 1)).append("! ").append(error(e, base)).append('\n'))
      case _         => ()
    }
    t.children.foreach(render(_, depth + 1, base, sb))
  }

  private def header(t: Tree, base: Int): String = {
    def s(x: Span) = s"${x.start - base}..${x.end - base}"
    def o(f: Formula) = if (f.outer == f.span) "" else s" outer ${s(f.outer)}"
    t match {
      case n: Name              => s"Name ${quote(n.text)} ${s(n.span)}"
      case op: Op[?]            => s"Op ${op.kind} ${s(op.span)}"
      case r: FofRecord         => s"FofRecord ${s(r.span)}" + r.annotations.fold("")(a => s" annotations ${s(a)}")
      case r: Include           => s"Include ${s(r.span)}" + (if (r.selection.isDefined) " selection" else "")
      case r: OtherRecord       => s"OtherRecord ${r.language} ${s(r.span)}"
      case r: ErrorRecord       => s"ErrorRecord ${cause(r.cause)} ${s(r.span)} ${quote(r.message)}"
      case r: Role              => s"Role ${s(r.span)}" + r.detail.fold("")(d => s" detail ${s(d)}")
      case l: Logical           => s"Logical ${s(l.span)}"
      case q: Sequent           => s"Sequent ${s(q.span)} left ${q.left.size}"
      case f: AtomicFormula     => s"AtomicFormula ${f.kind} ${s(f.span)}${o(f)}"
      case f: Equality          => s"Equality ${s(f.span)}${o(f)}"
      case f: Inequality        => s"Inequality ${s(f.span)}${o(f)}"
      case f: QuantifiedFormula => s"QuantifiedFormula ${s(f.span)}${o(f)}"
      case f: UnaryFormula      => s"UnaryFormula ${s(f.span)}${o(f)}"
      case f: BinaryFormula     => s"BinaryFormula ${s(f.span)}${o(f)}"
      case e: ErrorNode         => s"ErrorNode ${cause(e.cause)} ${s(e.span)}${o(e)} ${quote(e.message)}"
      case a: AtomicTerm        => s"AtomicTerm ${a.kind} ${s(a.span)}"
      case v: Variable          => s"Variable ${s(v.span)}"
      case d: DistinctObject    => s"DistinctObject ${quote(d.text)} ${s(d.span)}"
      case n: NumberTerm        => s"NumberTerm ${n.text} ${s(n.span)}"
    }
  }

  private def error(e: SyntaxError, base: Int): String =
    s"${cause(e.cause)} ${e.span.start - base}..${e.span.end - base} ${e.message}"

  private def kinds(ks: Set[TokenKind]): String = ks.toVector.map(_.name).sorted.mkString(", ")

  private def quote(s: String): String =
    "\"" + s.replace("\\", "\\\\").replace("\n", "\\n").replace("\r", "\\r").replace("\"", "\\\"") + "\""
}
