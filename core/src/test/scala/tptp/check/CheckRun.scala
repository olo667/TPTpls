package tptp.check

import java.nio.charset.StandardCharsets.ISO_8859_1
import java.nio.file.{Files, Path}
import tptp.analysis.{DiskSources, ProjectAnalyzer}
import tptp.mutation.Mutations
import tptp.semantics.*

/** Checker diagnostics over a TPTP library's FOF problems, by code. Not a test:
  * `sbt "core/Test/runMain tptp.check.CheckRun /path/to/TPTP [Problems|Axioms]"`, then report the numbers. Problems whose include
  * closure is over 64 MB are skipped (they need a heap of many GB); the analyzer's cache is cleared every 100
  * problems so included files do not accumulate. */
object CheckRun {
  private val MaxClosureBytes = 64L * 1024 * 1024
  private val IncludeLine = """(?m)^\s*include\(\s*'([^']+)'""".r

  def main(args: Array[String]): Unit = {
    val root = Path.of(args(0)).toAbsolutePath
    val env = ResolutionEnv(Vector.empty, Some(root))
    val dir = args.lift(1).getOrElse("Problems") // or "Axioms"
    val (files, large) = Mutations.paths(root.resolve(dir), Int.MaxValue, maxBytes = 1024L * 1024)
    val (checked, huge) = files.partition(f => closureBytes(root, f, Set.empty) <= MaxClosureBytes)
    val project = ProjectAnalyzer(DiskSources, env)
    val counts = scala.collection.mutable.Map.empty[DiagnosticCode, Int].withDefaultValue(0)
    val examples = scala.collection.mutable.Map.empty[DiagnosticCode, String]
    checked.zipWithIndex.foreach { case (f, i) =>
      if (i % 100 == 0) project.setEnv(env) // clears the cache
      project.analyzeRoot(f).foreach(_.diagnostics.foreach { d =>
        counts(d.code) += 1
        examples.getOrElseUpdate(d.code, s"${root.relativize(f)}: ${d.message}")
      })
    }
    println(s"${checked.size} files checked (${large.size} over 1 MB and ${huge.size} with includes over 64 MB skipped)")
    counts.toVector.sortBy(-_._2).foreach { case (c, n) => println(f"$n%8d  $c  e.g. ${examples(c)}") }
  }

  /** The size of a file and everything it includes, from its `include('…')` lines (relative to the TPTP root). */
  private def closureBytes(root: Path, f: Path, seen: Set[Path]): Long =
    if (seen(f) || !Files.isRegularFile(f)) 0L
    else {
      val includes = IncludeLine.findAllMatchIn(Files.readString(f, ISO_8859_1)).map(m => root.resolve(m.group(1))).toVector
      Files.size(f) + includes.map(closureBytes(root, _, seen + f)).sum
    }
}
