package tptp.mutation

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/** Full mutation run over a TPTP library:
  * {{{ sbt "core/Test/runMain tptp.mutation.MutationRun --tptp ~/.cache/tptp-lsp/TPTP-v9.3.1 --per-kind 5 --seed 1" }}}
  * Options: --tptp DIR (default $TPTP), --per-kind N (5), --seed S (1), --files N (all), --max-size-kb N (256),
  * --parallel-kb N (6144: total size of the files mutated at the same time, bounding memory),
  * --out DIR (target/mutation-report). Larger files are skipped and listed, since every mutant re-parses its file.
  * Writes report.md, mutants.csv, unparsable.txt and too-large.txt; exits with 1 if any invariant is violated. */
object MutationRun {
  def main(args: Array[String]): Unit = {
    val opts = args.grouped(2).collect { case Array(k, v) if k.startsWith("--") => k.drop(2) -> v }.toMap
    val root = Path.of(opts.get("tptp").orElse(sys.env.get("TPTP")).getOrElse(sys.error("--tptp DIR or TPTP must be given")))
    val perKind = opts.get("per-kind").map(_.toInt).getOrElse(5)
    val seed = opts.get("seed").map(_.toLong).getOrElse(1L)
    val limit = opts.get("files").map(_.toInt).getOrElse(Int.MaxValue)
    val maxKb = opts.get("max-size-kb").map(_.toLong).getOrElse(256L)
    val budgetKb = opts.get("parallel-kb").map(_.toInt).getOrElse(6144)
    val out = Path.of(opts.getOrElse("out", "target/mutation-report")).toAbsolutePath
    Files.createDirectories(out)

    val t0 = System.nanoTime
    val (files, tooLarge) = Mutations.paths(root, limit, maxKb * 1024)
    println(s"${files.size} files up to $maxKb KB, ${tooLarge.size} larger files skipped")
    val step = math.max(files.size / 50, 1)
    val (outcomes, unparsable) = Mutations.runStreaming(root, files, seed, perKind, budgetKb, n =>
      if (n % step == 0) println(f"  $n/${files.size} files, ${(System.nanoTime - t0) / 6e10}%.1f min"))
    val minutes = (System.nanoTime - t0) / 6e10
    val header =
      f"# Mutation report\n\nCorpus `$root`: ${files.size} files up to $maxKb KB (${tooLarge.size} larger files skipped, " +
        f"${unparsable.size} not parsing cleanly; see too-large.txt and unparsable.txt), $perKind mutants per kind and file, " +
        f"seed $seed, ${outcomes.size} mutants in $minutes%.1f min."
    Files.writeString(out.resolve("report.md"), MutationReport.markdown(outcomes, header), UTF_8)
    Files.writeString(out.resolve("mutants.csv"), MutationReport.csv(outcomes), UTF_8)
    Files.writeString(out.resolve("unparsable.txt"), unparsable.sorted.mkString("", "\n", "\n"), UTF_8)
    Files.writeString(out.resolve("too-large.txt"), tooLarge.map((n, s) => s"$n\t$s").mkString("", "\n", "\n"), UTF_8)
    println(MutationReport.summary(outcomes))
    println(s"Report: ${out.resolve("report.md")}")
    if (MutationReport.violations(outcomes).nonEmpty) sys.exit(1)
  }
}
