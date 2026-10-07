package tptp.syntax

enum ErrorCause {
  case MissingToken(expected: Set[TokenKind])
  case UnexpectedToken(found: TokenKind)
  case InputMismatch(expected: Set[TokenKind])
  case NoViableAlternative
  case IncompleteRule
  case LexerError
  /** A repair hole: an element such as a "term" or "formula" is missing here. */
  case MissingElement(what: String)
}

/** A syntax error as reported to the user. */
final case class SyntaxError(cause: ErrorCause, message: String, span: Span)

sealed trait Cst {
  def span: Span
}

/** A grammar rule application. `incomplete` is set when the rule ended with a recognition error. */
final case class Node(kind: RuleKind, span: Span, children: Vector[Cst], incomplete: Option[ErrorCause]) extends Cst {
  def nodes: Vector[Node] = children.collect { case n: Node => n }
  def tokens: Vector[Token] = children.collect { case t: Token => t }
  def child(kind: RuleKind): Option[Node] = nodes.find(_.kind == kind)
  def token(kind: TokenKind): Option[Token] = tokens.find(_.kind == kind)
  def hasErrors: Boolean =
    incomplete.isDefined || children.exists {
      case n: Node      => n.hasErrors
      case _: ErrorNode => true
      case _: Token     => false
    }
}

final case class Token(kind: TokenKind, text: String, span: Span) extends Cst

/** Something the parser could not fit into the grammar. Missing tokens have a zero-width span and no skipped tokens. */
final case class ErrorNode(cause: ErrorCause, message: String, span: Span, skipped: Vector[Token]) extends Cst

/** A parsed document: one top-level element per record (a `tptp_input` node or an error node). */
final case class CstFile(records: Vector[Cst], comments: Vector[Token], lines: LineIndex) {

  /** Elements from a top-level record down to the innermost element at `offset`; empty if none. */
  def pathAt(offset: Int): List[Cst] = {
    def pick(xs: Vector[Cst]): Option[Cst] =
      xs.find(_.span.contains(offset)).orElse(xs.find(_.span.touches(offset)))
    def descend(c: Cst): List[Cst] = c match {
      case n: Node      => n :: pick(n.children).map(descend).getOrElse(Nil)
      case e: ErrorNode => e :: pick(e.skipped).toList
      case t: Token     => List(t)
    }
    pick(records).map(descend).getOrElse(Nil)
  }
}
