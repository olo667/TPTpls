package tptp.mutation

import tptp.syntax.*

enum Precision {
  /** An error with the expected cause at the expected place. */
  case Exact
  /** The expected kind of error, but at another place (e.g. a deleted `)` that fits several places). */
  case Displaced
  /** Errors, but not of the expected kind. */
  case Elsewhere
  /** No error at all (only acceptable for typos, which may produce valid text). */
  case NoError
}

final case class CheckResult(violations: Vector[String], precision: Precision, errors: Vector[SyntaxError], millis: Long)

/** Parses a mutant and checks the invariants every mutant must satisfy, and how precise the errors are. */
object Check {
  val DefaultTimeLimitMs = 2000L

  def run(original: ParsedFile, m: Mutant, timeLimitMs: Long = DefaultTimeLimitMs): CheckResult = {
    val t0 = System.nanoTime
    val parsed =
      try Right(SyntaxParser.parse(m.text))
      catch { case e: Throwable => Left(e) } // includes StackOverflowError: a crash is a finding, not a test abort
    val ms = (System.nanoTime - t0) / 1000000
    parsed match {
      case Left(e) => CheckResult(Vector(s"parser threw $e"), Precision.NoError, Vector.empty, ms)
      case Right(p) =>
        val violations = Vector.newBuilder[String]
        if (ms > timeLimitMs) violations += s"took $ms ms (limit $timeLimitMs ms)"
        if (m.kind.mustError && p.errors.isEmpty) violations += "no error reported"
        val records = original.cst.records
        val boundaryStart = records.lift(m.boundaryRecord).map(_.span.start).getOrElse(0)
        p.errors.filter(_.span.start < boundaryStart).take(3).foreach { e =>
          violations += s"error before the mutated record: '${e.message}' at ${e.span.start}"
        }
        val before = records.take(m.boundaryRecord)
        if (p.cst.records.take(before.size) != before) violations += "records before the mutation changed"
        if (m.recordsAfter > 0) {
          val delta = p.cst.lines.length - original.cst.lines.length
          if (p.cst.records.takeRight(m.recordsAfter) != records.takeRight(m.recordsAfter).map(shift(_, delta)))
            violations += "records after the mutation changed"
        }
        val e = m.expected
        val precision =
          if (p.errors.isEmpty) Precision.NoError
          else if (p.errors.exists(x => x.span.start >= e.from && x.span.start <= e.to && e.cause(x.cause))) Precision.Exact
          else if (p.errors.exists(x => e.cause(x.cause)) && !m.kind.causeIsGeneric) Precision.Displaced
          else Precision.Elsewhere
        CheckResult(violations.result(), precision, p.errors, ms)
    }
  }

  private def shift(c: Cst, d: Int): Cst = {
    def s(span: Span) = Span(span.start + d, span.end + d)
    c match {
      case n: Node      => n.copy(span = s(n.span), children = n.children.map(shift(_, d)))
      case t: Token     => t.copy(span = s(t.span))
      case e: ErrorNode => e.copy(span = s(e.span), skipped = e.skipped.map(t => t.copy(span = s(t.span))))
    }
  }
}
