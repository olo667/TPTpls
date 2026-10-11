package tptp.syntax

enum ErrorCause {
  case MissingToken(expected: Set[TokenKind])
  case UnexpectedToken(found: TokenKind)
  case InputMismatch(expected: Set[TokenKind])
  case NoViableAlternative
  case IncompleteRule
  case LexerError
  /** A repair hole: an element such as a "term" or "formula" is missing here. */
  case MissingElement(what: Element)
  /** The contents of a bracket group that could not be parsed; the error node covers the group, `at` is the
    * token where parsing failed, which is where the diagnostic is shown. */
  case Unparsable(at: Span)
}

/** A syntax error as reported to the user. */
final case class SyntaxError(cause: ErrorCause, message: String, span: Span)
