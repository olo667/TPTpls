package tptp.syntax

import org.antlr.v4.runtime.{BaseErrorListener, CharStreams, LexerNoViableAltException, RecognitionException, Recognizer, Token as AntlrToken}
import tptp.syntax.generated.TPTPLexer

/** Comments are skipped by the grammar, so only default-channel tokens remain. */
final case class LexResult(tokens: Vector[AntlrToken], errors: Vector[SyntaxError])

/** Tokens are produced while `tokens` is iterated; `errors` is complete once it is exhausted. */
final class LexStream private[syntax] (lexer: TPTPLexer, collected: scala.collection.mutable.ArrayBuffer[SyntaxError]) {
  val tokens: Iterator[AntlrToken] = Iterator.continually(lexer.nextToken()).takeWhile(_.getType != AntlrToken.EOF)
  def errors: Vector[SyntaxError] = collected.toVector
}

object LexerDriver {

  /** All tokens at once, for tools and tests. The parser uses `stream`. */
  def lex(text: String): LexResult = {
    val s = stream(text)
    val tokens = s.tokens.toVector
    LexResult(tokens, s.errors)
  }

  /** Lexes lazily, so a record's tokens can be dropped before the next record is lexed. Never throws. */
  def stream(text: String): LexStream = {
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
    val errors = scala.collection.mutable.ArrayBuffer.empty[SyntaxError]
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
    new LexStream(lexer, errors)
  }
}

private[syntax] object AntlrTokens {
  def span(t: AntlrToken): Span =
    if (t.getType == AntlrToken.EOF) Span.empty(math.max(t.getStartIndex, 0))
    else Span(t.getStartIndex, t.getStopIndex + 1)
}
