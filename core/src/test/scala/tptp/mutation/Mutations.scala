package tptp.mutation

import java.nio.charset.StandardCharsets.ISO_8859_1
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using
import tptp.syntax.*

final case class CorpusFile(name: String, text: String, parsed: ParsedFile)

final case class Outcome(file: String, mutant: Mutant, result: CheckResult)

object Mutations {
  /** FOF problem and axiom files below `root` (TPTP names containing `+`). Files that do not parse
    * cleanly cannot serve as originals and are returned separately. */
  def corpus(root: Path, limit: Int = Int.MaxValue): (Vector[CorpusFile], Vector[String]) = {
    val paths = Using.resource(Files.walk(root)) {
      _.iterator.asScala.filter(p => Files.isRegularFile(p) && p.getFileName.toString.contains("+"))
        .filter(p => p.toString.endsWith(".p") || p.toString.endsWith(".ax")).toVector
    }.sortBy(_.toString).take(limit)
    val loaded = paths.map { p =>
      val text = Files.readString(p, ISO_8859_1)
      CorpusFile(root.relativize(p).toString, text, SyntaxParser.parse(text))
    }
    (loaded.filter(_.parsed.errors.isEmpty), loaded.filter(_.parsed.errors.nonEmpty).map(_.name))
  }

  /** Runs up to `perKind` mutants of every kind on every file, files in parallel. */
  def run(files: Vector[CorpusFile], seed: Long, perKind: Int): Vector[Outcome] =
    files.asJava.parallelStream().map(f => mutate(f, seed, perKind)).toList.asScala.toVector.flatten

  /** Time allowed for one mutant: grows with the file, since every mutant re-parses the whole file. */
  def timeLimitMs(text: String): Long = Check.DefaultTimeLimitMs + 4L * text.length / 1024

  private def mutate(f: CorpusFile, seed: Long, perKind: Int): Vector[Outcome] = {
    val limit = timeLimitMs(f.text)
    MutationKind.values.toVector.flatMap { kind =>
      Mutator(seed).mutants(f.text, f.parsed, kind, perKind).map { m =>
        val r = Check.run(f.parsed, m, limit)
        // keep outcomes small: the mutated text and most errors are not needed for the report
        Outcome(f.name, m.copy(text = ""), r.copy(errors = r.errors.take(3)))
      }
    }
  }

  /** The FOF files below `root`, split into those up to `maxBytes` and the larger ones (name, size). */
  def paths(root: Path, limit: Int, maxBytes: Long): (Vector[Path], Vector[(String, Long)]) = {
    val all = Using.resource(Files.walk(root)) {
      _.iterator.asScala.filter(p => Files.isRegularFile(p) && p.getFileName.toString.contains("+"))
        .filter(p => p.toString.endsWith(".p") || p.toString.endsWith(".ax")).toVector
    }.sortBy(_.toString).take(limit)
    val (small, large) = all.partition(Files.size(_) <= maxBytes)
    (small, large.map(p => root.relativize(p).toString -> Files.size(p)))
  }

  /** Streams files through mutation in parallel, keeping only one parsed file per thread in memory.
    * Returns the outcomes and the files that do not parse cleanly (they cannot serve as originals). */
  def runStreaming(root: Path, files: Vector[Path], seed: Long, perKind: Int, budgetKb: Int,
                   progress: Int => Unit): (Vector[Outcome], Vector[String]) = {
    val done = new java.util.concurrent.atomic.AtomicInteger()
    // Each file holds two full parse trees (~180 bytes per byte of text) while it is mutated, so the number of
    // files in flight is limited by their total size, not by thread count: many small files, few large ones.
    val memory = new java.util.concurrent.Semaphore(budgetKb)
    val results = files.asJava.parallelStream().map { p =>
      val permits = math.min(budgetKb, math.max(1, (Files.size(p) / 1024).toInt))
      memory.acquireUninterruptibly(permits)
      try {
      val text = Files.readString(p, ISO_8859_1)
      val parsed = SyntaxParser.parse(text)
      val name = root.relativize(p).toString
      val r: Either[String, Vector[Outcome]] =
        if (parsed.errors.nonEmpty) Left(name) else Right(mutate(CorpusFile(name, text, parsed), seed, perKind))
      progress(done.incrementAndGet())
      r
      } finally memory.release(permits)
    }.toList.asScala.toVector
    (results.collect { case Right(os) => os }.flatten, results.collect { case Left(n) => n })
  }
}

object MutationReport {
  private def pct(n: Int, total: Int) = if (total == 0) "-" else f"${100.0 * n / total}%.1f%%"

  def summary(outcomes: Vector[Outcome]): String = {
    val rows = MutationKind.values.toVector.map { kind =>
      val os = outcomes.filter(_.mutant.kind == kind)
      def count(p: Precision) = os.count(_.result.precision == p)
      f"| ${kind.label}%-20s | ${os.size}%7d | ${pct(count(Precision.Exact), os.size)}%7s | ${pct(count(Precision.Displaced), os.size)}%9s | ${pct(count(Precision.Elsewhere), os.size)}%9s | ${pct(count(Precision.NoError), os.size)}%8s | ${os.count(_.result.violations.nonEmpty)}%10d |"
    }
    ("| mutation             | mutants |   exact | displaced | elsewhere | no error | violations |" +:
      "|----------------------|--------:|--------:|----------:|----------:|---------:|-----------:|" +: rows).mkString("\n")
  }

  def violations(outcomes: Vector[Outcome]): Vector[String] =
    outcomes.filter(_.result.violations.nonEmpty).map(o =>
      s"${o.file}: ${o.mutant.description}: ${o.result.violations.mkString("; ")}")

  def markdown(outcomes: Vector[Outcome], header: String, examplesPerKind: Int = 15): String = {
    val notExact = MutationKind.values.toVector.flatMap { kind =>
      // displaced results are expected for some kinds; wrong kinds of errors are the interesting misses
      val misses = outcomes.filter(o => o.mutant.kind == kind && o.result.precision == Precision.Elsewhere).take(examplesPerKind)
      if (misses.isEmpty) Vector.empty
      else s"\n### ${kind.label}: errors of another kind (first $examplesPerKind)\n" +: misses.map { o =>
        val got = o.result.errors.take(2).map(e => s"${CstPrinter.cause(e.cause)} '${e.message}' at ${e.span.start}").mkString("; ")
        s"- `${o.file}`: ${o.mutant.description}; expected ${o.mutant.expected.describe} at ${o.mutant.expected.from}..${o.mutant.expected.to}; got ${if (got.isEmpty) "no error" else got}"
      }
    }
    val v = violations(outcomes)
    (Vector(header, "", summary(outcomes), "", s"## Invariant violations (${v.size})", "") ++ v.take(200).map("- " + _) ++
      Vector("", "## Imprecise results") ++ notExact).mkString("\n") + "\n"
  }

  def csv(outcomes: Vector[Outcome]): String = {
    def q(s: String) = "\"" + s.replace("\"", "\"\"") + "\""
    ("file,kind,mutation,precision,violations,millis" +: outcomes.map { o =>
      Seq(q(o.file), q(o.mutant.kind.label), q(o.mutant.description), o.result.precision.toString,
        q(o.result.violations.mkString("; ")), o.result.millis.toString).mkString(",")
    }).mkString("\n") + "\n"
  }
}
