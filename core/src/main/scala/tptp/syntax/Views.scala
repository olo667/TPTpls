package tptp.syntax

import Grammar.{Rules, Tokens}

object Names {
  /** TPTP name identity: a single-quoted word denotes the same name as its unquoted content. */
  def key(token: Token): String =
    if (token.kind == Tokens.singleQuoted) unquote(token.text) else token.text

  /** Strips the surrounding quotes and resolves `\\` and `\'` escapes. */
  def unquote(text: String): String = {
    val inner = if (text.length >= 2) text.substring(1, text.length - 1) else text
    val sb = new StringBuilder
    var i = 0
    while (i < inner.length) {
      val c = inner.charAt(i)
      if (c == '\\' && i + 1 < inner.length) { sb.append(inner.charAt(i + 1)); i += 2 }
      else { sb.append(c); i += 1 }
    }
    sb.toString
  }

  /** The word or integer token of a `name` node. */
  private[syntax] def nameToken(name: Node): Option[Token] =
    name.child(Rules.atomicWord).flatMap(_.tokens.headOption).orElse(name.tokens.headOption)
}

final case class AnnotatedFormula(node: Node) {
  def language: String = Rules.annotatedLanguages(node.kind)
  def keyword: Option[Token] = node.tokens.headOption
  def name: Option[Token] = node.child(Rules.name).flatMap(Names.nameToken)
  def role: Option[Token] = node.child(Rules.formulaRole).flatMap(_.tokens.headOption)
}

final case class IncludeDirective(node: Node) {
  def keyword: Option[Token] = node.tokens.headOption
  def fileNameToken: Option[Token] =
    node.child(Rules.fileName).flatMap(_.child(Rules.atomicWord)).flatMap(_.tokens.headOption)
  def fileName: Option[String] =
    fileNameToken.map(t => if (t.kind == Tokens.singleQuoted) Names.unquote(t.text) else t.text)

  /** `None` means everything is included (no selection, or `*`). */
  def selection: Option[Vector[Token]] =
    node
      .child(Rules.includeOptionals)
      .flatMap(_.child(Rules.formulaSelection))
      .flatMap(_.child(Rules.nameList))
      .map(nameListTokens)

  private def nameListTokens(list: Node): Vector[Token] =
    list.child(Rules.name).flatMap(Names.nameToken).toVector ++
      list.child(Rules.nameList).map(nameListTokens).getOrElse(Vector.empty)
}

enum Record {
  case Formula(view: AnnotatedFormula)
  case Include(view: IncludeDirective)
}

object Record {
  def of(cst: Cst): Option[Record] = cst match {
    case input: Node if input.kind == Rules.tptpInput =>
      input.nodes.headOption.flatMap { inner =>
        if (inner.kind == Rules.include) Some(Record.Include(IncludeDirective(inner)))
        else if (inner.kind == Rules.annotatedFormula)
          inner.nodes.headOption
            .filter(n => Rules.annotatedLanguages.contains(n.kind))
            .map(n => Record.Formula(AnnotatedFormula(n)))
        else None
      }
    case _ => None
  }
}
