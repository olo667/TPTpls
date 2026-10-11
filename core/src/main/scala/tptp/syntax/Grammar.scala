package tptp.syntax

/** The only place grammar token names appear; a grammar update that renames them fails here. Rules are used through
  * the generated context classes (`tree.Lowering`), so a renamed rule fails to compile. */
object Grammar {
  object Tokens {
    val recordKeywords: Set[TokenKind] =
      Set("tpi", "thf", "tff", "tcf", "fof", "cnf", "include").map(TokenKind.literal)
    val openParen: TokenKind = TokenKind.literal("(")
    val recordEnd: TokenKind = TokenKind.literal(".")
    val lowerWord: TokenKind = TokenKind.symbolic("Lower_word")
    val upperWord: TokenKind = TokenKind.symbolic("Upper_word")
    val singleQuoted: TokenKind = TokenKind.symbolic("Single_quoted")
  }

  /** Tokens the error repair may insert (§5.5 of the spec), in preference order. */
  object Repair {
    /** Placeholders for a missing term/formula/name (first) or variable (second). */
    val holes: Vector[TokenKind] = Vector(Tokens.lowerWord, Tokens.upperWord)
    val closers: Vector[TokenKind] = Vector(")", "]", ".").map(TokenKind.literal)
    val separators: Vector[TokenKind] = Vector(",", ":").map(TokenKind.literal)
  }
}
