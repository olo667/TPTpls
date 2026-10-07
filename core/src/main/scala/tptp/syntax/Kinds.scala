package tptp.syntax

import tptp.syntax.generated.TPTPParser

/** A grammar rule, identified by the generated parser's rule index. */
opaque type RuleKind = Int

object RuleKind {
  def apply(index: Int): RuleKind = index

  def named(name: String): RuleKind = {
    val i = TPTPParser.ruleNames.indexOf(name)
    if (i < 0) throw new IllegalArgumentException(s"unknown grammar rule '$name'")
    i
  }

  extension (k: RuleKind) {
    def index: Int = k
    def name: String = TPTPParser.ruleNames(k)
  }
}

/** A token type, identified by the generated lexer's token type. */
opaque type TokenKind = Int

object TokenKind {
  def apply(tokenType: Int): TokenKind = tokenType

  val Eof: TokenKind = org.antlr.v4.runtime.Token.EOF

  /** The implicit token for a literal in the grammar, e.g. `literal("fof(")`. */
  def literal(text: String): TokenKind = find(TPTPParser.VOCABULARY.getLiteralName(_) == s"'$text'", s"literal '$text'")

  /** The token of a named lexer rule, e.g. `symbolic("Lower_word")`. */
  def symbolic(name: String): TokenKind = find(TPTPParser.VOCABULARY.getSymbolicName(_) == name, s"token $name")

  private def find(p: Int => Boolean, what: String): TokenKind =
    (0 to TPTPParser.VOCABULARY.getMaxTokenType)
      .find(p)
      .getOrElse(throw new IllegalArgumentException(s"no $what in the grammar"))

  extension (k: TokenKind) {
    def tokenType: Int = k
    def name: String = TPTPParser.VOCABULARY.getDisplayName(k)
  }
}
