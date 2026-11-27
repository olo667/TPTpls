package tptp.syntax

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import tptp.syntax.generated.{TPTPLexer, TPTPParser}

class GeneratedParserSuite extends munit.FunSuite {
  private def parser(text: String): TPTPParser = new TPTPParser(new CommonTokenStream(new TPTPLexer(CharStreams.fromString(text))))

  test("generated parser parses a minimal FOF file") {
    val p = parser("fof(a, axiom, p).\n")
    val tree = p.tptp_file()
    assertEquals(p.getNumberOfSyntaxErrors, 0)
    assertEquals(tree.tptp_input().size, 1)
  }

  test("record keywords are names too") {
    val p = parser("fof(a, axiom, p(file, include, fof)).\ntff(t, type, tff: $i).\n")
    p.tptp_file()
    assertEquals(p.getNumberOfSyntaxErrors, 0)
  }

  /** Prediction lookahead over a TFF formula nested `depth` deep, in both directions. */
  private def lookahead(depth: Int): Long = {
    val right = "(p => " * depth + "q" + ")" * depth
    val left = "(" * depth + "q" + " => p)" * depth
    val p = parser(s"tff(r, axiom, $right).\ntff(l, axiom, $left).\n")
    p.setProfile(true)
    p.tptp_file()
    assertEquals(p.getNumberOfSyntaxErrors, 0)
    p.getParseInfo.getDecisionInfo.map(d => d.SLL_TotalLook + d.LL_TotalLook).sum
  }

  test("prediction lookahead grows linearly with TFF nesting") {
    val (small, large) = (lookahead(100), lookahead(400))
    assert(large < 5 * small, s"lookahead $small at depth 100, $large at depth 400")
  }
}
