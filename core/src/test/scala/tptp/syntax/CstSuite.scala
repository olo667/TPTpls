package tptp.syntax

class CstSuite extends munit.FunSuite {
  private val fofKw = Token(TokenKind.literal("fof("), "fof(", Span(0, 4))
  private val name = Token(TokenKind.symbolic("Lower_word"), "a", Span(4, 5))
  private val missingComma =
    ErrorNode(ErrorCause.MissingToken(Set(TokenKind.literal(","))), "missing ','", Span(5, 5), Vector.empty)
  private val node = Node(RuleKind.named("fof_annotated"), Span(0, 5), Vector(fofKw, name, missingComma), None)
  private val file = CstFile(Vector(node), Vector.empty, LineIndex("fof(a"))

  test("kinds resolve names from the generated parser") {
    assertEquals(RuleKind.named("fof_annotated").name, "fof_annotated")
    assertEquals(TokenKind.literal("fof(").name, "'fof('")
    assertEquals(TokenKind.symbolic("Lower_word").name, "Lower_word")
    intercept[IllegalArgumentException](RuleKind.named("no_such_rule"))
    intercept[IllegalArgumentException](TokenKind.literal("no such literal"))
  }

  test("printer renders nodes, tokens and error nodes") {
    val expected =
      """fof_annotated 0..5
        |  'fof(' "fof(" 0..4
        |  Lower_word "a" 4..5
        |  !error MissingToken(',') 5..5 "missing ','"
        |""".stripMargin
    assertEquals(CstPrinter.print(file), expected)
  }

  test("pathAt prefers the element containing the offset, then one touching it") {
    assertEquals(file.pathAt(4), List(node, name))
    assertEquals(file.pathAt(5), List(node, name))
    assertEquals(file.pathAt(99), Nil)
  }

  test("hasErrors sees error nodes and incomplete nodes") {
    assert(node.hasErrors)
    assert(!node.copy(children = Vector(fofKw, name)).hasErrors)
    assert(node.copy(children = Vector(fofKw), incomplete = Some(ErrorCause.NoViableAlternative)).hasErrors)
  }
}
