package tptp.syntax.tree

import org.antlr.v4.runtime.{ParserRuleContext, Token as AntlrToken}
import org.antlr.v4.runtime.tree.{ParseTree, TerminalNode}
import tptp.syntax.{AntlrTokens, ErrorCause, RepairToken, SyntaxError, TokenKind}

/** The syntax errors of a repaired record: one per repair token in its parse tree and one per deleted token,
  * in source order. Covers the whole record, including formulas that are not lowered. */
object RecordErrors {
  def ofRepair(tree: ParserRuleContext, deleted: Vector[AntlrToken]): Vector[SyntaxError] = {
    val out = Vector.newBuilder[SyntaxError]
    def visit(t: ParseTree): Unit = t match {
      case n: TerminalNode =>
        n.getSymbol match {
          case r: RepairToken => out ++= ofRepairToken(n, r)
          case _              => ()
        }
      case c: ParserRuleContext => (0 until c.getChildCount).foreach(i => visit(c.getChild(i)))
      case _                    => ()
    }
    visit(tree)
    out ++= deleted.map { t =>
      val kind = TokenKind(t.getType)
      SyntaxError(ErrorCause.UnexpectedToken(kind), s"unexpected '${t.getText}'", AntlrTokens.span(t))
    }
    out.result().sortBy(_.span.start)
  }

  private def ofRepairToken(n: TerminalNode, r: RepairToken): Option[SyntaxError] = r.origin match {
    case RepairToken.Origin.Hole =>
      val what = Lowering.holeElement(n.getParent.asInstanceOf[ParserRuleContext], r)
      Some(SyntaxError(ErrorCause.MissingElement(what), s"missing ${what.label}", AntlrTokens.span(r)))
    case RepairToken.Origin.Collapsed =>
      // reported where parsing failed, not at the start of the whole group
      Some(SyntaxError(ErrorCause.Unparsable(r.failedAt), "could not parse this part", r.failedAt))
    case RepairToken.Origin.Inserted =>
      val kind = TokenKind(r.getType)
      Some(SyntaxError(ErrorCause.MissingToken(Set(kind)), s"missing ${kind.name}", AntlrTokens.span(r)))
  }
}
