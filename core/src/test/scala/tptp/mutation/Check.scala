package tptp.mutation

import tptp.syntax.{SyntaxError, SyntaxParser}
import tptp.syntax.tree.{Record, TptpFile, TreePrinter}

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

  def run(original: TptpFile, m: Mutant, timeLimitMs: Long = DefaultTimeLimitMs): CheckResult = {
    val t0 = System.nanoTime
    val parsed =
      try Right(SyntaxParser.parse(m.text))
      catch { case e: Throwable => Left(e) } // includes StackOverflowError: a crash is a finding, not a test abort
    val ms = (System.nanoTime - t0) / 1000000
    parsed match {
      case Left(e) => CheckResult(Vector(s"parser threw $e"), Precision.NoError, Vector.empty, ms)
      case Right(p) =>
        val errors = p.allErrors
        val violations = Vector.newBuilder[String]
        if (ms > timeLimitMs) violations += s"took $ms ms (limit $timeLimitMs ms)"
        if (m.kind.mustError && errors.isEmpty) violations += "no error reported"
        val records = original.records
        val boundaryStart = records.lift(m.boundaryRecord).map(_.span.start).getOrElse(0)
        errors.filter(_.span.start < boundaryStart).take(3).foreach { e =>
          violations += s"error before the mutated record: '${e.message}' at ${e.span.start}"
        }
        if (printed(p.records.take(m.boundaryRecord)) != printed(records.take(m.boundaryRecord)))
          violations += "records before the mutation changed"
        if (m.recordsAfter > 0 && printed(p.records.takeRight(m.recordsAfter)) != printed(records.takeRight(m.recordsAfter)))
          violations += "records after the mutation changed"
        val e = m.expected
        val precision =
          if (errors.isEmpty) Precision.NoError
          else if (errors.exists(x => x.span.start >= e.from && x.span.start <= e.to && e.cause(x.cause))) Precision.Exact
          else if (errors.exists(x => e.cause(x.cause)) && !m.kind.causeIsGeneric) Precision.Displaced
          else Precision.Elsewhere
        CheckResult(violations.result(), precision, errors, ms)
    }
  }

  /** Records printed with spans relative to their own start, so records that moved compare equal. */
  private def printed(records: Vector[Record]): Vector[String] = records.map(r => TreePrinter.print(r, r.span.start))

}
