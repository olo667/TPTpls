package tptp.syntax

import org.antlr.v4.runtime.{BailErrorStrategy, CommonTokenStream, ListTokenSource, Token as AntlrToken}
import org.antlr.v4.runtime.atn.PredictionMode
import org.antlr.v4.runtime.misc.ParseCancellationException
import tptp.syntax.generated.TPTPParser

import scala.jdk.CollectionConverters.*

final case class ParsedFile(cst: CstFile, errors: Vector[SyntaxError])

object SyntaxParser {

  /** Parses a whole document. Never throws on malformed input. Afterwards the shared prediction cache is
    * trimmed to `cacheLimit`, so long-running use (the server, mutation runs) has bounded memory. */
  def parse(text: String, cacheLimit: PredictionCache.Limit = PredictionCache.DefaultLimit): ParsedFile = {
    val lexed = LexerDriver.lex(text)
    val records = Vector.newBuilder[Cst]
    val errors = Vector.newBuilder[SyntaxError] ++= lexed.errors
    val repairBudget = new RepairBudget
    for (segment <- RecordSplitter.split(lexed.tokens)) {
      segment match {
        case Segment.Junk(tokens) =>
          val first = tokens.head.getText
          val e = junk(tokens, s"expected a record such as fof(...) or include(...), found '$first'")
          records += e
          errors += SyntaxError(e.cause, e.message, e.span)
        case Segment.Record(tokens) =>
          val (node, recordErrors) = parseRecord(tokens, repairBudget)
          records += node
          errors ++= recordErrors
      }
    }
    val parsed = ParsedFile(CstFile(records.result(), lexed.comments.map(AntlrTokens.toCst), LineIndex(text)), errors.result())
    PredictionCache.trim(cacheLimit)
    parsed
  }

  private def parseRecord(tokens: Vector[AntlrToken], repairBudget: RepairBudget): (Node, Vector[SyntaxError]) = {
    val start = tokens.head.getStartIndex
    fastParse(tokens) match {
      case Right(tree) => (CstBuilder.build(tree, new ErrorCauseStrategy, start), Vector.empty)
      case Left(complete) =>
        // a record that parsed but left trailing tokens keeps the trailing-junk handling
        val repaired = if (complete) None else RecordRepair.repair(tokens, repairBudget)
        repaired match {
          case Some(repair) => buildRepaired(repair, start)
          case None         => recoverByDefault(tokens, start)
        }
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

  private def buildRepaired(repair: RecordRepair.Repair, start: Int): (Node, Vector[SyntaxError]) = {
    val tree = fastParse(repair.tokens).getOrElse(throw new IllegalStateException("repaired record does not parse"))
    val built = CstBuilder.build(tree, new ErrorCauseStrategy, start)
    val node = repair.deleted.foldLeft(built) { (n, t) =>
      val tok = AntlrTokens.toCst(t)
      insertByPosition(n, ErrorNode(ErrorCause.UnexpectedToken(tok.kind), s"unexpected '${tok.text}'", tok.span, Vector(tok)))
    }
    (node, errorNodes(node).map(e => SyntaxError(e.cause, e.message, e.span)))
  }

  /** Puts `e` into the innermost node whose span strictly encloses it, in source order. */
  private def insertByPosition(node: Node, e: ErrorNode): Node = {
    val inner = node.children.indexWhere {
      case n: Node => n.span.start < e.span.start && e.span.end < n.span.end
      case _       => false
    }
    if (inner >= 0) node.copy(children = node.children.updated(inner, insertByPosition(node.children(inner).asInstanceOf[Node], e)))
    else {
      val at = node.children.indexWhere(_.span.start >= e.span.end) match { case -1 => node.children.size; case i => i }
      node.copy(children = node.children.patch(at, Vector(e), 0), span = node.span.cover(e.span))
    }
  }

  private def errorNodes(c: Cst): Vector[ErrorNode] = c match {
    case n: Node      => n.children.flatMap(errorNodes)
    case e: ErrorNode => Vector(e)
    case _: Token     => Vector.empty
  }

  private def recoverByDefault(tokens: Vector[AntlrToken], start: Int): (Node, Vector[SyntaxError]) = {
    val strategy = new ErrorCauseStrategy
    val slow = newParser(tokens)
    slow.setErrorHandler(strategy)
    val node = CstBuilder.build(slow.tptp_input(), strategy, start)
    val trailing = remaining(slow)
    if (trailing.isEmpty) (node, strategy.errors)
    else {
      val e = junk(trailing, s"unexpected '${trailing.head.getText}' after the end of the record")
      val withTrailing = node.copy(children = node.children :+ e, span = node.span.cover(e.span))
      // report trailing tokens only if recovery inside the record reported nothing
      val extra = if (strategy.errors.isEmpty) Vector(SyntaxError(e.cause, e.message, e.span)) else Vector.empty
      (withTrailing, strategy.errors ++ extra)
    }
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

  private def junk(tokens: Vector[AntlrToken], message: String): ErrorNode = {
    val cst = tokens.map(AntlrTokens.toCst)
    ErrorNode(ErrorCause.UnexpectedToken(cst.head.kind), message, cst.head.span.cover(cst.last.span), cst)
  }
}
