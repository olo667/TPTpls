package tptp.syntax

import org.antlr.v4.runtime.{BaseErrorListener, CharStreams, LexerNoViableAltException, RecognitionException, Recognizer, Token as AntlrToken}
import tptp.syntax.generated.TPTPLexer

import scala.jdk.CollectionConverters.*

final case class LexResult(tokens: Vector[AntlrToken], comments: Vector[AntlrToken], errors: Vector[SyntaxError])

object LexerDriver {

  /** Lexes the whole text. Never throws on malformed input. */
  def lex(text: String): LexResult = {
    val length = text.codePointCount(0, text.length)
    val lexer = new TPTPLexer(CharStreams.fromString(text))
    lexer.removeErrorListeners()
    val errors = Vector.newBuilder[SyntaxError]
    lexer.addErrorListener(new BaseErrorListener {
      override def syntaxError(
          recognizer: Recognizer[?, ?],
          offendingSymbol: AnyRef,
          line: Int,
          charPositionInLine: Int,
          msg: String,
          e: RecognitionException,
      ): Unit = {
        val start = e match {
          case n: LexerNoViableAltException => n.getStartIndex
          case _                            => lexer._tokenStartCharIndex
        }
        errors += SyntaxError(ErrorCause.LexerError, msg, Span(start, math.min(start + 1, length).max(start)))
      }
    })
    val all = lexer.getAllTokens.asScala.toVector
    LexResult(
      all.filter(_.getChannel == AntlrToken.DEFAULT_CHANNEL),
      all.filter(_.getChannel == Grammar.Channels.comments),
      errors.result(),
    )
  }
}

private[syntax] object AntlrTokens {
  def span(t: AntlrToken): Span =
    if (t.getType == AntlrToken.EOF) Span.empty(math.max(t.getStartIndex, 0))
    else Span(t.getStartIndex, t.getStopIndex + 1)

  def toCst(t: AntlrToken): Token = Token(TokenKind(t.getType), t.getText, span(t))
}
