package tptp.syntax

/** The only place grammar rule and token names appear. A grammar update that renames them fails here. */
object Grammar {
  object Rules {
    val tptpInput: RuleKind = RuleKind.named("tptp_input")
    val annotatedFormula: RuleKind = RuleKind.named("annotated_formula")
    val include: RuleKind = RuleKind.named("include")
    val name: RuleKind = RuleKind.named("name")
    val atomicWord: RuleKind = RuleKind.named("atomic_word")
    val formulaRole: RuleKind = RuleKind.named("formula_role")
    val fileName: RuleKind = RuleKind.named("file_name")
    val includeOptionals: RuleKind = RuleKind.named("include_optionals")
    val formulaSelection: RuleKind = RuleKind.named("formula_selection")
    val nameList: RuleKind = RuleKind.named("name_list")

    /** Annotated-formula rule → language keyword. */
    val annotatedLanguages: Map[RuleKind, String] =
      Vector("thf", "tff", "tcf", "fof", "cnf", "tpi").map(l => RuleKind.named(s"${l}_annotated") -> l).toMap
  }

  object Channels {
    /** The grammar sends comments to channel 2 (combined grammars cannot declare named channels). */
    val comments: Int = 2
  }

  object Tokens {
    val recordKeywords: Set[TokenKind] =
      Set("tpi(", "thf(", "tff(", "tcf(", "fof(", "cnf(", "include(").map(TokenKind.literal)
    val lowerWord: TokenKind = TokenKind.symbolic("Lower_word")
    val upperWord: TokenKind = TokenKind.symbolic("Upper_word")
    val singleQuoted: TokenKind = TokenKind.symbolic("Single_quoted")
  }

  /** Tokens the error repair may insert (§5.5 of the spec), in preference order. */
  object Repair {
    /** Placeholders for a missing term/formula/name (first) or variable (second). */
    val holes: Vector[TokenKind] = Vector(Tokens.lowerWord, Tokens.upperWord)
    val closers: Vector[TokenKind] = Vector(")", "]", ").").map(TokenKind.literal)
    val separators: Vector[TokenKind] = Vector(",", ":").map(TokenKind.literal)
  }

  /** What kind of element a rule stands for, used in "missing …" messages. */
  object Categories {
    def describe(rule: RuleKind): String = {
      val name = rule.name
      if (name.contains("role")) "role" // before "formula": the rule is formula_role
      else if (name.contains("formula")) "formula"
      else if (name.contains("variable")) "variable"
      else if (name.contains("term") || name.contains("argument")) "term"
      else if (name == "name" || name.endsWith("_name")) "name"
      else name.replace('_', ' ')
    }
  }
}
