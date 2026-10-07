package tptp.syntax

import org.antlr.v4.runtime.atn.{ParserATNSimulator, PredictionContextCache}
import org.antlr.v4.runtime.dfa.DFA
import tptp.syntax.generated.TPTPParser

/** ANTLR's prediction cache (DFA states and prediction contexts), owned here so that it can be bounded.
  * The generated parser's own cache is static and never shrinks: parsing many different inputs, broken
  * ones and repair candidates in particular, grows it without limit. Thread-safe. */
object PredictionCache {
  final case class Limit(states: Int)
  val DefaultLimit: Limit = Limit(100000)

  private final class Cache {
    val dfa: Array[DFA] = Array.tabulate(TPTPParser._ATN.getNumberOfDecisions)(i => new DFA(TPTPParser._ATN.getDecisionState(i), i))
    val contexts = new PredictionContextCache
  }

  @volatile private var current = new Cache
  @volatile private var resetCount = 0

  /** Makes `parser` predict with the current cache instead of the generated parser's static one. */
  def install(parser: TPTPParser): TPTPParser = {
    val c = current
    parser.setInterpreter(new ParserATNSimulator(parser, TPTPParser._ATN, c.dfa, c.contexts))
    parser
  }

  def states: Int = current.dfa.iterator.map(_.states.size).sum
  def resets: Int = resetCount

  /** Starts over with an empty cache; parsers already running keep the old one until they finish. */
  def reset(): Unit = synchronized { current = new Cache; resetCount += 1 }

  /** Resets the cache if it has grown beyond `limit`. Called after each parsed document. */
  def trim(limit: Limit): Unit = if (states > limit.states) synchronized { if (states > limit.states) reset() }
}
