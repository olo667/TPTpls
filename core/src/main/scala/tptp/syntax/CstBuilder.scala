package tptp.syntax

import org.antlr.v4.runtime.{InputMismatchException, NoViableAltException, ParserRuleContext, RecognitionException, Token as AntlrToken}
import org.antlr.v4.runtime.tree.{ErrorNode as AntlrErrorNode, ParseTreeListener, ParseTreeWalker, TerminalNode}

/** Converts an ANTLR parse tree into a CST. Never refers to individual grammar rules. */
final class CstBuilder private (strategy: ErrorCauseStrategy, startOffset: Int) extends ParseTreeListener {
  private final class Frame(val ctx: ParserRuleContext) {
    val children = Vector.newBuilder[Cst]
    var last: Option[Cst] = None
    def add(c: Cst): Unit = (last, c) match {
      // merge runs of tokens skipped for the same reason into one error node
      case (Some(prev: ErrorNode), next: ErrorNode)
          if prev.cause == next.cause && prev.skipped.nonEmpty && next.skipped.nonEmpty =>
        last = Some(prev.copy(span = prev.span.cover(next.span), skipped = prev.skipped ++ next.skipped))
      case _ =>
        last.foreach(children += _)
        last = Some(c)
    }
    def result(): Vector[Cst] = { last.foreach(children += _); last = None; children.result() }
  }

  private var stack: List[Frame] = Nil
  private var lastEnd = startOffset
  private var root: Option[Node] = None

  override def enterEveryRule(ctx: ParserRuleContext): Unit = stack = new Frame(ctx) :: stack

  override def exitEveryRule(ctx: ParserRuleContext): Unit = {
    val children = stack.head.result()
    stack = stack.tail
    val span =
      if (children.isEmpty) Span.empty(lastEnd) else Span(children.head.span.start, children.last.span.end)
    val node = Node(RuleKind(ctx.getRuleIndex), span, children, Option(ctx.exception).map(causeOf))
    stack match {
      case parent :: _ => parent.add(node)
      case Nil         => root = Some(node)
    }
  }

  override def visitTerminal(node: TerminalNode): Unit = node.getSymbol match {
    case r: RepairToken if r.origin == RepairToken.Origin.Hole =>
      val what = holeCategory(r)
      stack.head.add(ErrorNode(ErrorCause.MissingElement(what), s"missing $what", Span.empty(r.getStartIndex), Vector.empty))
    case r: RepairToken if r.origin == RepairToken.Origin.Inserted =>
      val kind = TokenKind(r.getType)
      stack.head.add(ErrorNode(ErrorCause.MissingToken(Set(kind)), s"missing ${kind.name}", Span.empty(r.getStartIndex), Vector.empty))
    case t if t.getType != AntlrToken.EOF =>
      stack.head.add(AntlrTokens.toCst(t))
      lastEnd = t.getStopIndex + 1
    case _ => ()
  }

  /** The element a hole stands for: the outermost enclosing rule that consists of the hole alone. */
  private def holeCategory(hole: AntlrToken): String =
    stack.iterator
      .map(_.ctx)
      .takeWhile(ctx => (ctx.getStart eq hole) && (ctx.getStop eq hole))
      .toVector
      .lastOption
      .map(ctx => Grammar.Categories.describe(RuleKind(ctx.getRuleIndex)))
      .getOrElse("element")

  override def visitErrorNode(node: AntlrErrorNode): Unit = {
    val t = node.getSymbol
    val error =
      if (t.getTokenIndex == -1) {
        val kind = TokenKind(t.getType)
        ErrorNode(ErrorCause.MissingToken(Set(kind)), s"missing ${kind.name}", Span.empty(lastEnd), Vector.empty)
      } else if (t.getType == AntlrToken.EOF) {
        ErrorNode(ErrorCause.NoViableAlternative, "unexpected end of record", Span.empty(lastEnd), Vector.empty)
      } else {
        val tok = AntlrTokens.toCst(t)
        lastEnd = tok.span.end
        val (cause, message) = strategy
          .causeOfSkipped(t.getTokenIndex)
          .getOrElse((ErrorCause.UnexpectedToken(tok.kind), s"unexpected '${tok.text}'"))
        ErrorNode(cause, message, tok.span, Vector(tok))
      }
    stack.head.add(error)
  }

  private def causeOf(e: RecognitionException): ErrorCause = e match {
    case m: InputMismatchException => ErrorCause.InputMismatch(ErrorCauseStrategy.kinds(m.getExpectedTokens))
    case _: NoViableAltException   => ErrorCause.NoViableAlternative
    case _                         => ErrorCause.IncompleteRule
  }
}

object CstBuilder {
  def build(tree: ParserRuleContext, strategy: ErrorCauseStrategy, startOffset: Int): Node = {
    val builder = new CstBuilder(strategy, startOffset)
    ParseTreeWalker.DEFAULT.walk(builder, tree)
    builder.root.getOrElse(throw new IllegalStateException("parse tree produced no root"))
  }
}
