package tptp.syntax.languages

/** FOF's roles and defined words, written by hand from `SyntaxBNF-v9.3.1.6` (next to this file): its `:==`
  * rules give the general lists, its prose narrows them per language. Check against the BNF when updating it. */
object Fof {
  /** Line 83, `<formula_role> :==`. */
  val bnfRoles: Set[String] = Set("axiom", "hypothesis", "definition", "assumption", "lemma", "theorem", "corollary",
    "conjecture", "negated_conjecture", "plain", "type", "interpretation", "logic", "unknown")

  /** Narrowed by the prose after line 85: "type"s define a symbol's type (typed languages only), `logic` is for
    * modal logic specifications, "unknown"s are "an error situation". */
  val roles: Set[String] = bnfRoles -- Set("type", "logic", "unknown")

  /** Line 503, `<defined_proposition> :== $true | $false`. */
  val definedPropositions: Set[String] = Set("$true", "$false")

  /** Lines 505–506 list `$distinct` and the arithmetic predicates; the prose after them puts `$distinct` in TXF and
    * THF, and arithmetic belongs to the typed languages. None is defined in FOF. */
  val definedPredicates: Set[String] = Set.empty

  /** Lines 520–524 list the arithmetic functors, which belong to the typed languages. None is defined in FOF. */
  val definedFunctors: Set[String] = Set.empty

  /** Lines 503, 505–506 and 520–524 together, to tell "not defined in FOF" from "not a defined word". */
  val bnfDefinedWords: Set[String] = Set("$true", "$false", "$distinct", "$less", "$lesseq", "$greater", "$greatereq",
    "$is_int", "$is_rat", "$uminus", "$sum", "$difference", "$product", "$quotient", "$quotient_e", "$quotient_t",
    "$quotient_f", "$remainder_e", "$remainder_t", "$remainder_f", "$floor", "$ceiling", "$truncate", "$round", "$abs",
    "$to_int", "$to_rat", "$to_real")
}
