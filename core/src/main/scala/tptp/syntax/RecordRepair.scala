package tptp.syntax

import org.antlr.v4.runtime.{BailErrorStrategy, CharStreams, CommonToken, CommonTokenStream, ListTokenSource, NoViableAltException, RecognitionException, Token as AntlrToken}
import org.antlr.v4.runtime.atn.PredictionMode
import org.antlr.v4.runtime.misc.ParseCancellationException
import tptp.syntax.generated.{TPTPLexer, TPTPParser}

import scala.jdk.CollectionConverters.*

/** A token created by a repair: an inserted hole or punctuation token, one part of a split token, or a
  * placeholder standing for the `collapsed` contents of a bracket group that could not be parsed. */
final class RepairToken(tokenType: Int, text: String, start: Int, stop: Int, val origin: RepairToken.Origin,
                        val collapsed: Vector[AntlrToken] = Vector.empty, val failedAt: Span = Span.empty(0))
    extends CommonToken(tokenType, text) {
  setStartIndex(start)
  setStopIndex(stop)
}

/** Repair work allowed for one whole file, shared by its records (in token-parses). */
final class RepairBudget(private var left: Long = RecordRepair.FileBudget) {
  def remaining: Long = left
  private[syntax] def spend(units: Long): Unit = left -= units
}

object RepairToken {
  enum Origin {
    case Hole, Inserted, SplitPart, Collapsed
  }
}

/** Finds a small set of token edits that makes a failing record parse (spec §5.5). */
object RecordRepair {

  /** A successful repair: the edited token sequence and the tokens it deleted. */
  final case class Repair(tokens: Vector[AntlrToken], deleted: Vector[AntlrToken])

  val MaxEdits = 3
  /** Bracket groups that may be collapsed in one record (spec §5.5, bracket containment). */
  val MaxCollapses = 3
  /** Budget in token-parses (parses × record length), so the search stays bounded on long records. */
  val WorkBudget = 300000L
  /** Budget for all records of one file; later broken records fall back to ANTLR's default recovery. */
  val FileBudget = 1000000L

  /** Literal tokens made only of brackets and `.` that split into two tokens, e.g. `[]` → `[` `]`.
    * Operators such as `<=>` are deliberately not split: a gap inside an operator is never meaningful. */
  val splits: Map[TokenKind, (TokenKind, TokenKind)] = {
    val vocab = TPTPParser.VOCABULARY
    def single(text: String): Option[TokenKind] = {
      val ts = new TPTPLexer(CharStreams.fromString(text)).getAllTokens.asScala
      if (ts.size == 1 && ts.head.getText == text) Some(TokenKind(ts.head.getType)) else None
    }
    (1 to vocab.getMaxTokenType).flatMap { t =>
      Option(vocab.getLiteralName(t)).map(_.stripPrefix("'").stripSuffix("'")).filter { text =>
        text.length >= 2 && text.forall("()[]{}.".contains(_))
      }.flatMap { text =>
        (1 until text.length).iterator.flatMap { i =>
          for (a <- single(text.take(i)); b <- single(text.drop(i))) yield TokenKind(t) -> (a, b)
        }.nextOption()
      }
    }.toMap
  }

  private enum Candidate(val kind: TokenKind, val cost: Int, val order: Int) {
    case Hole(k: TokenKind, o: Int) extends Candidate(k, 1, o)
    case Punct(k: TokenKind, c: Int, o: Int) extends Candidate(k, c, o)
  }

  private val candidates: Vector[Candidate] = {
    val holes = Grammar.Repair.holes.map(k => Candidate.Hole(k, 0))
    val closers = Grammar.Repair.closers.map(k => Candidate.Punct(k, 1, 0))
    val separators = Grammar.Repair.separators.map(k => Candidate.Punct(k, 3, 0))
    (holes ++ closers ++ separators).zipWithIndex.map {
      case (Candidate.Hole(k, _), i)     => Candidate.Hole(k, i)
      case (Candidate.Punct(k, c, _), i) => Candidate.Punct(k, c, i)
    }
  }
  private val DeleteCost = 2
  private val DeleteOrder = candidates.size

  /** A partial repair: current tokens, deleted tokens, cost, number of deletions, and a rank tie-breaker. */
  private final case class State(tokens: Vector[AntlrToken], deleted: Vector[AntlrToken], cost: Int, deletions: Int,
                                 positions: Int, order: Vector[Int])

  private final class Budget(perParse: Int, limit: Long) {
    private var left = limit
    def take(): Boolean = { left -= perParse; left >= 0 }
    def exhausted: Boolean = left < 0
    def used: Long = limit - math.max(left, 0)
  }

  /** Where the parse fails: `None` if the tokens form a complete record. */
  private final case class Failure(window: Vector[Int])

  def repair(tokens: Vector[AntlrToken], file: RepairBudget = new RepairBudget): Option[Repair] =
    if (file.remaining <= 0) None
    else {
      val perParse = math.max(1, tokens.size)
      val limit = math.min(WorkBudget, file.remaining)
      // the cheap search may use half of the budget, so that bracket containment always gets a turn
      val cheap = new Budget(perParse, limit / 2)
      try deepen(tokens, cheap).orElse {
        val rest = new Budget(perParse, limit - cheap.used)
        try contain(tokens, rest, MaxCollapses)
        finally file.spend(rest.used)
      }
      finally file.spend(cheap.used)
    }

  /** Iterative deepening: the cheapest repair with the fewest edits, up to MaxEdits. */
  private def deepen(tokens: Vector[AntlrToken], budget: Budget, maxDepth: Int = MaxEdits): Option[Repair] = {
    val initial = State(tokens, Vector.empty, 0, 0, 0, Vector.empty)
    var depth = 1
    var best: Option[State] = None
    while (best.isEmpty && depth <= maxDepth) {
      val found = search(initial, depth, budget)
      if (found.isEmpty && budget.exhausted) depth = maxDepth + 1
      best = found
      depth += 1
    }
    best.map(s => Repair(s.tokens, s.deleted))
  }

  /** A bracket group: the indices of its opening and closing token. */
  private final case class Group(open: Int, close: Int)

  /** Balanced groups opened by a standalone `(` or `[` and closed by `)`, `]` or the `)` of `).`.
    * Keyword tokens such as `fof(` do not open groups; unbalanced brackets yield no group. */
  private def groups(tokens: Vector[AntlrToken]): Vector[Group] = {
    var stack = List.empty[(Int, Char)]
    val found = Vector.newBuilder[Group]
    for ((t, i) <- tokens.zipWithIndex) t.getText match {
      case "(" => stack = (i, '(') :: stack
      case "[" => stack = (i, '[') :: stack
      case ")" | ")." | "]" =>
        val kind = if (t.getText == "]") '[' else '('
        stack match {
          case (open, `kind`) :: rest => found += Group(open, i); stack = rest
          case _                      => ()
        }
      case _ => ()
    }
    found.result()
  }

  /** Bracket containment: when no small set of edits repairs the record, replace the contents of the innermost
    * bracket group around the error by one placeholder, then repair what is left (another collapse included). */
  private def contain(tokens: Vector[AntlrToken], budget: Budget, collapsesLeft: Int): Option[Repair] =
    if (collapsesLeft == 0 || !budget.take()) None
    else
      failure(tokens) match {
        case None => Some(Repair(tokens, Vector.empty))
        case Some(Failure(window)) =>
          val point = window.last
          groups(tokens)
            .filter(g => g.open < point && point <= g.close && g.close - g.open > 1)
            .sortBy(g => g.close - g.open)
            .iterator
            .flatMap { g =>
              val inside = tokens.slice(g.open + 1, g.close)
              val holes = if (tokens(g.open).getText == "[") Grammar.Repair.holes.reverse else Grammar.Repair.holes
              val failedAt = AntlrTokens.span(tokens(point))
              holes.iterator.map { kind =>
                val start = inside.head.getStartIndex
                val hole = new RepairToken(kind.tokenType, "", start, start - 1, RepairToken.Origin.Collapsed, inside, failedAt)
                tokens.patch(g.open + 1, Vector(hole), inside.size)
              }
            }
            .flatMap { collapsed =>
              if (budget.take() && failure(collapsed).isEmpty) Some(Repair(collapsed, Vector.empty))
              // what is left after a collapse is usually a missing bracket: a shallow search, then further collapses
              else deepen(collapsed, budget, maxDepth = 2).orElse(contain(collapsed, budget, collapsesLeft - 1))
            }
            .nextOption()
      }

  /** The best repair using exactly `depth` more edits from `state`, or none. */
  private def search(state: State, depth: Int, budget: Budget): Option[State] =
    if (!budget.take()) None
    else
      failure(state.tokens) match {
        case None                       => if (depth == 0) Some(state) else None
        case Some(_) if depth == 0      => None
        case Some(Failure(window)) =>
          val results = for {
            pos <- window.iterator
            next <- edits(state, pos)
            result <- search(next, depth - 1, budget)
          } yield result
          results.toVector.sortBy(rank).headOption
      }

  private def rank(s: State): (Int, Int, Int, String) =
    (s.cost, s.deletions, -s.positions, s.order.map(i => f"$i%03d").mkString)

  private def edits(state: State, pos: Int): Iterator[State] = {
    val ts = state.tokens
    val at = if (pos < ts.size) ts(pos).getStartIndex else if (ts.isEmpty) 0 else ts.last.getStopIndex + 1
    def made(c: Candidate, start: Int): AntlrToken = c match {
      case Candidate.Hole(k, _)     => new RepairToken(k.tokenType, "", start, start - 1, RepairToken.Origin.Hole)
      case Candidate.Punct(k, _, _) => new RepairToken(k.tokenType, k.name.stripPrefix("'").stripSuffix("'"), start, start - 1, RepairToken.Origin.Inserted)
    }
    def next(newTokens: Vector[AntlrToken], c: Candidate) =
      state.copy(tokens = newTokens, cost = state.cost + c.cost, positions = state.positions + pos, order = state.order :+ c.order)
    val inserts = candidates.iterator.map(c => next(ts.patch(pos, Vector(made(c, at)), 0), c))
    val deletes =
      if (pos < ts.size)
        Iterator.single(state.copy(tokens = ts.patch(pos, Nil, 1), deleted = state.deleted :+ ts(pos), cost = state.cost + DeleteCost,
          deletions = state.deletions + 1, positions = state.positions + pos, order = state.order :+ DeleteOrder))
      else Iterator.empty
    val splitInserts =
      if (pos < ts.size) splits.get(TokenKind(ts(pos).getType)).iterator.flatMap { case (a, b) =>
        val whole = ts(pos)
        val mid = whole.getStartIndex + a.name.stripPrefix("'").stripSuffix("'").length
        val left = new RepairToken(a.tokenType, whole.getText.take(mid - whole.getStartIndex), whole.getStartIndex, mid - 1, RepairToken.Origin.SplitPart)
        val right = new RepairToken(b.tokenType, whole.getText.drop(mid - whole.getStartIndex), mid, whole.getStopIndex, RepairToken.Origin.SplitPart)
        candidates.iterator.map(c => next(ts.patch(pos, Vector(left, made(c, mid), right), 1), c))
      }
      else Iterator.empty
    inserts ++ splitInserts ++ deletes
  }

  private def failure(tokens: Vector[AntlrToken]): Option[Failure] = {
    val parser = PredictionCache.install(new TPTPParser(new CommonTokenStream(new ListTokenSource(tokens.asJava))))
    parser.removeErrorListeners()
    parser.getInterpreter.setPredictionMode(PredictionMode.SLL)
    parser.setErrorHandler(new BailErrorStrategy)
    try {
      parser.tptp_input()
      val current = parser.getCurrentToken
      if (current.getType == AntlrToken.EOF) None else Some(Failure(Vector(current.getTokenIndex)))
    } catch {
      case e: ParseCancellationException =>
        val window = e.getCause match {
          case n: NoViableAltException =>
            val o = n.getOffendingToken.getTokenIndex
            Vector(n.getStartToken.getTokenIndex, o - 1, o)
          case r: RecognitionException =>
            val o = r.getOffendingToken.getTokenIndex
            Vector(o - 1, o)
          case _ => Vector(tokens.size)
        }
        Some(Failure(window.filter(i => i >= 0 && i <= tokens.size).distinct.sorted))
    }
  }
}
