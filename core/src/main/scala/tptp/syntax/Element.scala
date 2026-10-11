package tptp.syntax

/** What a repair hole stands for, used in "missing …" messages. */
enum Element(val label: String) {
  case Formula extends Element("formula")
  case Term extends Element("term")
  case Variable extends Element("variable")
  case Name extends Element("name")
  case Role extends Element("role")
  case FileName extends Element("file name")
  /** Any other rule, named after it: "cnf literal", "thf top level type", "source". */
  case Other(rule: String) extends Element(rule.replace('_', ' '))
}

object Element {
  /** The element a grammar rule stands for, judged by its name. */
  def forRule(rule: RuleKind): Element = {
    val name = rule.name
    if (name.contains("role")) Role // before "formula": the rule is formula_role
    else if (name == "file_name") FileName
    else if (name.contains("formula")) Formula
    else if (name.contains("variable")) Variable
    else if (name.contains("term") || name.contains("argument")) Term
    else if (name == "name" || name.endsWith("_name")) Name
    else Other(name)
  }
}
