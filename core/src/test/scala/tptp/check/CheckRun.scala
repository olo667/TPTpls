package tptp.check

import java.nio.charset.StandardCharsets.ISO_8859_1
import java.nio.file.{Files, Path}
import tptp.analysis.{DiskSources, ProjectAnalyzer, SourceProvider, Stamp}
import tptp.mutation.Mutations
import tptp.semantics.*

/** Checker diagnostics over a TPTP library's problems, by code, with timings. Not a test:
  * `sbt "core/Test/runMain tptp.check.CheckRun /path/to/TPTP [Problems|Axioms] [fof|tff]"`, then report the numbers.
  * Problems whose include closure is over 64 MB are skipped (they need a heap of many GB). Each problem starts cold
  * (the analyzer's cache cleared): its initial analysis is timed, then two edits of the root (one in a formula, one in
  * a type declaration) with the includes cached. The limits of the TFF design (initial < 1 min, edit p95 < 10 ms) hold
  * for problems with a root < 64 KB and includes < 64 MB; the others are reported separately. */
object CheckRun {
  private val MaxClosureBytes = 64L * 1024 * 1024
  private val MaxRootBytes = 64L * 1024
  private val IncludeLine = """(?m)^\s*include\(\s*'([^']+)'""".r

  /** The root's text replaced in memory; everything else from disk. Each new text gets a new version stamp. */
  private final class EditedSources(root: Path) extends SourceProvider {
    private var current: Option[(String, Stamp)] = None
    private var version = 0
    def set(text: String): Unit = { version += 1; current = Some((text, Stamp.Version(version))) }
    def reset(): Unit = current = None
    def stamp(path: Path): Option[Stamp] = if (path == root) current.map(_._2).orElse(DiskSources.stamp(path)) else DiskSources.stamp(path)
    def read(path: Path): Option[(String, Stamp)] = if (path == root) current.orElse(DiskSources.read(path)) else DiskSources.read(path)
  }

  private final case class Timing(problem: String, ms: Double)

  def main(args: Array[String]): Unit = {
    val root = Path.of(args(0)).toAbsolutePath
    val env = ResolutionEnv(Vector.empty, Some(root))
    val dir = args.lift(1).getOrElse("Problems") // or "Axioms"
    val marker = if (args.lift(2).contains("tff")) "_" else "+"
    val (files, large) = Mutations.paths(root.resolve(dir), Int.MaxValue, maxBytes = 1024L * 1024, marker)
    val (checked, huge) = files.partition(f => closureBytes(root, f, Set.empty) <= MaxClosureBytes)
    val counts = scala.collection.mutable.Map.empty[DiagnosticCode, Int].withDefaultValue(0)
    val examples = scala.collection.mutable.Map.empty[DiagnosticCode, String]
    val initial = Vector.newBuilder[(Timing, Boolean)]
    val edits = Vector.newBuilder[(Timing, Boolean)]
    checked.foreach { f =>
      val name = root.relativize(f).toString
      val within = Files.size(f) < MaxRootBytes && closureBytes(root, f, Set.empty) - Files.size(f) < MaxClosureBytes
      val sources = new EditedSources(f)
      val project = ProjectAnalyzer(sources, env) // a cold start: the problem's includes are parsed and folded anew
      val t0 = System.nanoTime()
      val result = project.analyzeRoot(f)
      initial += ((Timing(name, (System.nanoTime() - t0) / 1e6), within))
      result.foreach(_.diagnostics.foreach { d =>
        counts(d.code) += 1
        examples.getOrElseUpdate(d.code, s"$name: ${d.message}")
      })
      editedTexts(Files.readString(f, ISO_8859_1)).foreach { text =>
        sources.set(text)
        val t1 = System.nanoTime()
        project.analyzeRoot(f)
        edits += ((Timing(name, (System.nanoTime() - t1) / 1e6), within))
      }
    }
    println(s"${checked.size} files checked (${large.size} over 1 MB and ${huge.size} with includes over 64 MB skipped)")
    counts.toVector.sortBy(-_._2).foreach { case (c, n) => println(f"$n%8d  $c  e.g. ${examples(c)}") }
    report("within the limits", initial.result().filter(_._2).map(_._1), edits.result().filter(_._2).map(_._1))
    report("above the limits (not held to them)", initial.result().filterNot(_._2).map(_._1), edits.result().filterNot(_._2).map(_._1))
  }

  private def report(label: String, initial: Vector[Timing], edits: Vector[Timing]): Unit = {
    def p95(ts: Vector[Timing]) = ts.map(_.ms).sorted.lift(math.max(0, (ts.size * 95 + 99) / 100 - 1)).getOrElse(0.0)
    def max(ts: Vector[Timing]) = ts.maxByOption(_.ms).fold("-")(t => f"${t.ms}%.1f ms (${t.problem})")
    println(f"$label: ${initial.size} problems; initial: max ${max(initial)}, p95 ${p95(initial)}%.1f ms; " +
      f"edits: ${edits.size}, p95 ${p95(edits)}%.2f ms, max ${max(edits)}")
  }

  /** Two edits of a problem: a space before the closing `).` of the last non-type record, and of the last type record. */
  private def editedTexts(text: String): Vector[String] = {
    val Record = """(?m)^(tff|fof|thf|tcf|cnf)\([^,]*,\s*(\w+)""".r
    val records = Record.findAllMatchIn(text).map(m => (m.start, m.group(2))).toVector
    def endOf(start: Int): Option[Int] = {
      val i = text.indexOf(").", start)
      Option.when(i >= 0)(i)
    }
    Vector(records.filter(_._2 != "type").lastOption, records.filter(_._2 == "type").lastOption).flatten
      .flatMap { case (start, _) => endOf(start) }
      .map(i => text.substring(0, i) + " " + text.substring(i))
  }

  /** The size of a file and everything it includes, from its `include('…')` lines (relative to the TPTP root). */
  private def closureBytes(root: Path, f: Path, seen: Set[Path]): Long =
    if (seen(f) || !Files.isRegularFile(f)) 0L
    else {
      val includes = IncludeLine.findAllMatchIn(Files.readString(f, ISO_8859_1)).map(m => root.resolve(m.group(1))).toVector
      Files.size(f) + includes.map(closureBytes(root, _, seen + f)).sum
    }
}
