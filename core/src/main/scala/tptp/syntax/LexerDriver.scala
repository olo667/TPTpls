package tptp.syntax

import org.antlr.v4.runtime.{BaseErrorListener, CharStreams, LexerNoViableAltException, RecognitionException, Recognizer, Token as AntlrToken}
import tptp.syntax.generated.TPTPLexer

import scala.jdk.CollectionConverters.*

/** Comments are skipped by the grammar, so only default-channel tokens remain. */
final case class LexResult(tokens: Vector[AntlrToken], errors: Vector[SyntaxError])

object LexerDriver {

  /** Lexes the whole text. Never throws on malformed input. */
  def lex(text: String): LexResult = {
    val length = text.codePointCount(0, text.length)
    val lexer = new TPTPLexer(CharStreams.fromString(text)) {
      // ANTLR skips everything it read before failing, e.g. a whole line after an unterminated quote;
      // skip only the first character instead and lex the rest normally
      override def recover(e: LexerNoViableAltException): Unit = {
        getInputStream.seek(e.getStartIndex)
        getInterpreter.consume(getInputStream)
      }
    }
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
        val span = Span(start, math.min(start + 1, length).max(start))
        val message =
          if (span.length == 0) msg
          else s"unexpected character '${new String(Character.toChars(text.codePointAt(text.offsetByCodePoints(0, start))))}'"
        errors += SyntaxError(ErrorCause.LexerError, message, span)
      }
    })
    LexResult(lexer.getAllTokens.asScala.toVector, errors.result())
  }
}

private[syntax] object AntlrTokens {
  def span(t: AntlrToken): Span =
    if (t.getType == AntlrToken.EOF) Span.empty(math.max(t.getStartIndex, 0))
    else Span(t.getStartIndex, t.getStopIndex + 1)

  def toCst(t: AntlrToken): Token = Token(TokenKind(t.getType), t.getText, span(t))
}
