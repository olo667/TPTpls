package tptp.check

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using
import tptp.analysis.{DiskSources, ProjectAnalyzer}
import tptp.semantics.*

/** The committed sample of valid TPTP FOF files must raise no checker errors. */
class CheckerCorpusSuite extends munit.FunSuite {
  private val root = Path.of("src/test/resources/corpus")
  private val checkerErrors = Set(DiagnosticCode.ArityMismatch, DiagnosticCode.PredicateFunctionClash,
    DiagnosticCode.ConflictsInInclude, DiagnosticCode.FreeVariable, DiagnosticCode.InvalidRole)

  test("valid FOF problems and axioms raise no checker errors") {
    val files = Using.resource(Files.walk(root))(_.iterator.asScala.filter(Files.isRegularFile(_)).toVector.sortBy(_.toString))
    assertEquals(files.size, 24, "the committed sample")
    val project = ProjectAnalyzer(DiskSources, ResolutionEnv(Vector.empty, Some(root.toAbsolutePath)))
    val all = files.flatMap(f => project.analyzeRoot(f.toAbsolutePath).toVector.flatMap(_.diagnostics.map(f -> _)))
    val errors = all.filter { case (_, d) => checkerErrors(d.code) }
    assert(errors.isEmpty, errors.take(10).map { case (f, d) => s"${root.relativize(f)}: ${d.message}" }.mkString("\n"))
    val warnings = all.collect { case (_, d) if d.severity == Severity.Warning => d.code }.groupBy(identity).view.mapValues(_.size).toMap
    println(s"checker warnings on the sample: $warnings")
  }
}
