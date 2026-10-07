package tptp.syntax

import org.antlr.v4.runtime.*
import org.antlr.v4.runtime.misc.IntervalSet

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** ANTLR's default recovery, plus a recorded cause for every reported error and every skipped token. */
final class ErrorCauseStrategy extends DefaultErrorStrategy {
  import ErrorCauseStrategy.*

  private val reported = Vector.newBuilder[SyntaxError]
  private val skipped = mutable.Map.empty[Int, (ErrorCause, String)]
  private var lastCause: (ErrorCause, String) = (ErrorCause.NoViableAlternative, "syntax error")

  def errors: Vector[SyntaxError] = reported.result()
  def causeOfSkipped(tokenIndex: Int): Option[(ErrorCause, String)] = skipped.get(tokenIndex)

  override protected def reportNoViableAlternative(r: Parser, e: NoViableAltException): Unit =
    record(ErrorCause.NoViableAlternative, s"unexpected ${describe(e.getOffendingToken)}", AntlrTokens.span(e.getOffendingToken))

  override protected def reportInputMismatch(r: Parser, e: InputMismatchException): Unit = {
    val expected = kinds(e.getExpectedTokens)
    record(
      ErrorCause.InputMismatch(expected),
      s"expected ${list(expected)}, found ${describe(e.getOffendingToken)}",
      AntlrTokens.span(e.getOffendingToken),
    )
  }

  override protected def reportFailedPredicate(r: Parser, e: FailedPredicateException): Unit =
    record(ErrorCause.IncompleteRule, e.getMessage, AntlrTokens.span(e.getOffendingToken))

  override protected def reportUnwantedToken(r: Parser): Unit =
    if (!inErrorRecoveryMode(r)) {
      beginErrorCondition(r)
      val t = r.getCurrentToken
      val cause = ErrorCause.UnexpectedToken(TokenKind(t.getType))
      val message = s"unexpected ${describe(t)}"
      skipped(t.getTokenIndex) = (cause, message)
      record(cause, message, AntlrTokens.span(t))
    }

  override protected def reportMissingToken(r: Parser): Unit =
    if (!inErrorRecoveryMode(r)) {
      beginErrorCondition(r)
      val expected = kinds(getExpectedTokens(r))
      val previous = r.getInputStream.LT(-1)
      val at = if (previous == null) AntlrTokens.span(r.getCurrentToken).start else previous.getStopIndex + 1
      record(ErrorCause.MissingToken(expected), s"missing ${list(expected)}", Span.empty(at))
    }

  override protected def consumeUntil(r: Parser, set: IntervalSet): Unit = {
    var ttype = r.getInputStream.LA(1)
    while (ttype != Token.EOF && !set.contains(ttype)) {
      skipped(r.getCurrentToken.getTokenIndex) = lastCause
      r.consume()
      ttype = r.getInputStream.LA(1)
    }
  }

  private def record(cause: ErrorCause, message: String, span: Span): Unit = {
    lastCause = (cause, message)
    reported += SyntaxError(cause, message, span)
  }
}

object ErrorCauseStrategy {
  def kinds(set: IntervalSet): Set[TokenKind] =
    set.toList.asScala.map(_.intValue).filter(_ >= Token.EOF).map(TokenKind(_)).toSet

  def list(kinds: Set[TokenKind]): String = {
    val names = kinds.toVector.map(k => if (k == TokenKind.Eof) "end of record" else k.name).sorted
    if (names.size == 1) names.head else s"one of ${names.mkString(", ")}"
  }

  def describe(t: Token): String =
    if (t == null || t.getType == Token.EOF) "end of record" else s"'${t.getText}'"
}
