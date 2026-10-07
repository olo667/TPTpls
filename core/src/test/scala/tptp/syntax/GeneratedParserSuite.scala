package tptp.syntax

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import tptp.syntax.generated.{TPTPLexer, TPTPParser}

class GeneratedParserSuite extends munit.FunSuite {
  test("generated parser parses a minimal FOF file") {
    val lexer = new TPTPLexer(CharStreams.fromString("fof(a, axiom, p).\n"))
    val parser = new TPTPParser(new CommonTokenStream(lexer))
    val tree = parser.tptp_file()
    assertEquals(parser.getNumberOfSyntaxErrors, 0)
    assertEquals(tree.tptp_input().size, 1)
  }
}
