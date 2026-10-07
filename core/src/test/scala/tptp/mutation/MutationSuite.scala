package tptp.mutation

import java.nio.file.Path

/** Mutation tests over the committed TPTP sample. Invariant violations fail; precision is reported. */
class MutationSuite extends munit.FunSuite {
  test("mutants of the TPTP sample satisfy all invariants") {
    val (files, unparsable) = Mutations.corpus(Path.of("src/test/resources/corpus"))
    assertEquals(unparsable, Vector.empty[String], "every sample file must parse cleanly")
    val outcomes = Mutations.run(files, seed = 1, perKind = 8)
    println(s"Mutation results for ${files.size} sample files:\n" + MutationReport.summary(outcomes))
    val violations = MutationReport.violations(outcomes)
    assert(violations.isEmpty, s"${violations.size} invariant violations, e.g.\n${violations.take(20).mkString("\n")}")
  }
}
