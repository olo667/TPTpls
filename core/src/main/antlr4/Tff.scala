package tptp.syntax.languages

/** TFF's roles, defined types and defined words for TF0, TF1 and TXF with arithmetic, written by hand from
  * `SyntaxBNF-v9.3.1.6` (next to this file). Check against the BNF when updating it. */
object Tff {
  /** Line 83 `<formula_role>`, without `unknown` ("an error situation"); `type` is for type declarations, `logic` for
    * the logic specifications of NX0 (non-classical TFF). */
  val roles: Set[String] = Fof.bnfRoles - "unknown"

  /** Line 492 `<defined_type> :==`, each mapped to its canonical name: the prose after it makes `$oType` and
    * `$iType` other names of `$o` and `$i`. */
  val definedTypes: Map[String, String] = Map("$o" -> "$o", "$oType" -> "$o", "$i" -> "$i", "$iType" -> "$i",
    "$tType" -> "$tType", "$int" -> "$int", "$rat" -> "$rat", "$real" -> "$real")

  /** Lines 495–497: `$int`, `$rat` and `$real` are the types of integers, rationals and reals. */
  val numericTypes: Set[String] = Set("$int", "$rat", "$real")

  /** Line 503, `<defined_proposition> :== $true | $false`. */
  val definedPropositions: Set[String] = Set("$true", "$false")

  /** How an arithmetic word is typed (TPTP arithmetic; the BNF only lists the words). */
  enum Arithmetic {
    case Compare            // two arguments of one numeric type → $o
    case Test               // one argument of any numeric type → $o
    case Same(arity: Int)   // arguments of one numeric type → that type
    case Quotient           // two arguments of one type, $rat or $real → that type
    case To(target: String) // one argument of any numeric type → `target`
  }

  /** Lines 505–506 (predicates, without `$distinct`) and 520–524 (functors). */
  val arithmetic: Map[String, Arithmetic] = {
    import Arithmetic.*
    Map("$less" -> Compare, "$lesseq" -> Compare, "$greater" -> Compare, "$greatereq" -> Compare,
      "$is_int" -> Test, "$is_rat" -> Test,
      "$uminus" -> Same(1), "$sum" -> Same(2), "$difference" -> Same(2), "$product" -> Same(2), "$quotient" -> Quotient,
      "$quotient_e" -> Same(2), "$quotient_t" -> Same(2), "$quotient_f" -> Same(2),
      "$remainder_e" -> Same(2), "$remainder_t" -> Same(2), "$remainder_f" -> Same(2),
      "$floor" -> Same(1), "$ceiling" -> Same(1), "$truncate" -> Same(1), "$round" -> Same(1), "$abs" -> Same(1),
      "$to_int" -> To("$int"), "$to_rat" -> To("$rat"), "$to_real" -> To("$real"))
  }

  /** TXF words: `$ite` (line 264, `<txf_conditional>`) and `$distinct` (prose after line 506: "part of the TXF and THF
    * languages"). */
  val txfWords: Set[String] = Set("$ite", "$distinct")

  val definedPredicates: Set[String] =
    arithmetic.collect { case (w, Arithmetic.Compare | Arithmetic.Test) => w }.toSet ++ txfWords

  val definedFunctors: Set[String] = (arithmetic.keySet -- definedPredicates) + "$ite"
}
