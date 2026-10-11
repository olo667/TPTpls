package tptp.syntax

import org.antlr.v4.runtime.{BailErrorStrategy, CommonTokenStream, ListTokenSource, Token as AntlrToken}
import org.antlr.v4.runtime.atn.PredictionMode
import org.antlr.v4.runtime.misc.ParseCancellationException
import tptp.syntax.generated.TPTPParser

import scala.jdk.CollectionConverters.*
import tptp.syntax.tree.{ErrorRecord, Lowering, Record, RecordErrors, TptpFile}

object SyntaxParser {

  /** Parses a whole document into the compact tree. Never throws on malformed input. Each record's parse tree
    * is lowered and dropped before the next record is parsed. Afterwards the shared prediction cache is trimmed to
    * `cacheLimit`, so long-running use (the server, mutation runs) has bounded memory. */
  def parse(text: String, cacheLimit: PredictionCache.Limit = PredictionCache.DefaultLimit): TptpFile = {
    val lexed = LexerDriver.stream(text)
    val records = Vector.newBuilder[Record]
    val repairBudget = new RepairBudget
    for (segment <- RecordSplitter.split(lexed.tokens)) {
      records += (segment match {
        case Segment.Junk(tokens)   => junkRecord(tokens)
        case Segment.Record(tokens) => lowerRecord(tokens, repairBudget)
      })
    }
    val file = TptpFile(records.result(), lexed.errors, LineIndex(text))
    PredictionCache.trim(cacheLimit)
    file
  }

  private def junkRecord(tokens: Vector[AntlrToken]): Record = {
    val cause = ErrorCause.UnexpectedToken(TokenKind(tokens.head.getType))
    val message = s"expected a record such as fof(...) or include(...), found '${tokens.head.getText}'"
    val span = AntlrTokens.span(tokens.head).cover(AntlrTokens.span(tokens.last))
    ErrorRecord(cause, message, Vector(SyntaxError(cause, message, span)), span)
  }

  private def lowerRecord(tokens: Vector[AntlrToken], repairBudget: RepairBudget): Record =
    fastParse(tokens) match {
      case Right(tree) => Lowering.record(tree, Vector.empty, None)
      case Left(complete) =>
        // a record that parsed but left trailing tokens keeps the trailing-junk handling
        val repaired = if (complete) None else RecordRepair.repair(tokens, repairBudget)
        repaired match {
          case Some(repair) =>
            val tree = fastParse(repair.tokens).getOrElse(throw new IllegalStateException("repaired record does not parse"))
            Lowering.record(tree, RecordErrors.ofRepair(tree, repair.deleted), None)
          case None => lowerByDefault(tokens)
        }
    }

  private def lowerByDefault(tokens: Vector[AntlrToken]): Record = {
    val strategy = new ErrorCauseStrategy
    val slow = newParser(tokens)
    slow.setErrorHandler(strategy)
    val tree = slow.tptp_input()
    val trailing = remaining(slow)
    if (trailing.isEmpty) Lowering.record(tree, strategy.errors, None)
    else {
      val span = AntlrTokens.span(trailing.head).cover(AntlrTokens.span(trailing.last))
      val message = s"unexpected '${trailing.head.getText}' after the end of the record"
      // report trailing tokens only if recovery inside the record reported nothing
      val extra =
        if (strategy.errors.isEmpty) Vector(SyntaxError(ErrorCause.UnexpectedToken(TokenKind(trailing.head.getType)), message, span))
        else Vector.empty
      Lowering.record(tree, strategy.errors ++ extra, Some(span))
    }
  }

  /** `Right(tree)` if the record parses completely; `Left(true)` if it parsed but tokens remain, `Left(false)` on an error. */
  private def fastParse(tokens: Vector[AntlrToken]): Either[Boolean, TPTPParser.Tptp_inputContext] = {
    val fast = newParser(tokens)
    fast.getInterpreter.setPredictionMode(PredictionMode.SLL)
    fast.setErrorHandler(new BailErrorStrategy)
    try {
      val tree = fast.tptp_input()
      if (fast.getCurrentToken.getType == AntlrToken.EOF) Right(tree) else Left(true)
    } catch { case _: ParseCancellationException => Left(false) }
  }

  private def newParser(tokens: Vector[AntlrToken]): TPTPParser = {
    val parser = PredictionCache.install(new TPTPParser(new CommonTokenStream(new ListTokenSource(tokens.asJava))))
    parser.removeErrorListeners()
    parser
  }

  private def remaining(parser: TPTPParser): Vector[AntlrToken] = {
    val stream = parser.getInputStream.asInstanceOf[CommonTokenStream]
    stream.fill()
    (stream.index until stream.size).map(stream.get).filter(_.getType != AntlrToken.EOF).toVector
  }

}
