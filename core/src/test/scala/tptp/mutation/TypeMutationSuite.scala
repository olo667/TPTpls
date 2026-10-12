package tptp.mutation

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Type mutants of the committed sample: every conflict and unbound variable is found, controls stay clean. */
class TypeMutationSuite extends munit.FunSuite {
  private val tptpRoot = Path.of("src/test/resources/corpus").toAbsolutePath

  test("type mutants of the sample are found, and controls raise no checker errors") {
    val files = Using.resource(Files.walk(tptpRoot))(_.iterator.asScala.filter(Files.isRegularFile(_)).toVector.sortBy(_.toString))
    val outcomes = files.flatMap(f => TypeMutations.run(tptpRoot, f, seed = 1, perKind = 10))
    println(s"Type mutation results for ${files.size} sample files:\n${TypeMutations.table(outcomes)}")
    val violations = outcomes.flatMap(o => o.violations.map(v => s"${o.file}: ${o.mutant.description}: $v"))
    val missed = outcomes.filter(_.precision == TypePrecision.Missed).map(o => s"${o.file}: ${o.mutant.description}")
    assert(violations.isEmpty, s"${violations.size} violations, e.g.\n${violations.take(20).mkString("\n")}")
    assert(missed.isEmpty, s"${missed.size} missed, e.g.\n${missed.take(20).mkString("\n")}")
  }
}
