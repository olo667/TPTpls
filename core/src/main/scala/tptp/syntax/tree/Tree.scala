package tptp.syntax.tree

import tptp.syntax.{ErrorCause, LineIndex, Span, SyntaxError}

// The whole node hierarchy is in this file, so that `Tree` and its families can be sealed: matches on any of them
// are checked for exhaustiveness, and no node class can be added elsewhere.

/** A node of the compact syntax tree. */
sealed trait Tree {
  def span: Span
  /** The child nodes, in field order. Hand-written per class; `TreeWalkSuite` checks it against the fields. */
  def children: Seq[Tree]
}

/** A connective or quantifier with its position. */
final case class Op[K](kind: K, span: Span) extends Tree {
  def children: Seq[Tree] = Nil
}

enum Quantifier { case ForAll, Exists }

enum UnaryConnective { case Not }

enum BinaryConnective { case Iff, Impl, If, Niff, Nor, Nand, Or, And }

object BinaryConnective {
  def fromText(text: String): Option[BinaryConnective] = text match {
    case "<=>" => Some(Iff)
    case "=>"  => Some(Impl)
    case "<="  => Some(If)
    case "<~>" => Some(Niff)
    case "~|"  => Some(Nor)
    case "~&"  => Some(Nand)
    case "|"   => Some(Or)
    case "&"   => Some(And)
    case _     => None
  }
}

/** Plain (`p`), defined (`$less`) or system (`$$p`) symbol, as the grammar distinguishes them. */
enum AtomKind { case Plain, Defined, System }

object AtomKind {
  def of(functor: String): AtomKind =
    if (functor.startsWith("$$")) System else if (functor.startsWith("$")) Defined else Plain
}

/** A name position (formula name, role, file name, selected name, quantified variable): the name, or an
  * `ErrorNode` where it is missing or unparsable. */
sealed trait NameNode extends Tree {
  /** The name, if this is not an error. */
  def valid: Option[Name] = this match {
    case n: Name      => Some(n)
    case _: ErrorNode => None
  }
}

/** A name as written, quotes included: formula names, roles, functors, variables, file names. */
final case class Name(text: String, span: Span) extends NameNode {
  def children: Seq[Tree] = Nil
  /** TPTP name identity: a single-quoted word denotes the same name as its unquoted content. */
  def key: String = if (text.length >= 2 && text.head == '\'') Name.unquote(text) else text
}

object Name {
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
}

sealed trait Statement extends Tree

final case class Logical(formula: Formula, span: Span) extends Statement {
  def children: Seq[Tree] = Vector(formula)
}

/** FOFX sequent: `[left] --> [right]`. */
final case class Sequent(left: Seq[Formula], right: Seq[Formula], span: Span) extends Statement {
  def children: Seq[Tree] = left ++ right
}

/** A FOF formula. `outer` is its extent including enclosing parentheses (`span` if there are none). */
sealed trait Formula extends Tree {
  def outer: Span
  def withOuter(outer: Span): Formula
  /** A `fof_unitary_formula`: quantified, atomic, or any formula in parentheses. */
  def isUnitary: Boolean = this.isInstanceOf[FofUnitaryFormula] || outer != span
}

// Chain rules of the grammar (rules whose alternatives are single rule references), as supertypes.
sealed trait FofLogicFormula extends Formula
sealed trait FofBinaryFormula extends FofLogicFormula
sealed trait FofUnitFormula extends FofLogicFormula
sealed trait FofUnaryFormula extends FofUnitFormula
sealed trait FofUnitaryFormula extends FofUnitFormula
sealed trait FofAtomicFormula extends FofUnitaryFormula

/** `p`, `p(a)`, `$true`, `$less(a, b)`, `$$p(a)`: plain, defined and system atoms, told apart by `kind`. */
final case class AtomicFormula(f: Name, args: Seq[Term], span: Span, outer: Span) extends FofAtomicFormula {
  def kind: AtomKind = AtomKind.of(f.text)
  def children: Seq[Tree] = f +: args
  def withOuter(outer: Span): Formula = copy(outer = outer)
}

final case class Equality(left: Term, right: Term, span: Span, outer: Span) extends FofAtomicFormula {
  def children: Seq[Tree] = Vector(left, right)
  def withOuter(outer: Span): Formula = copy(outer = outer)
}

final case class Inequality(left: Term, right: Term, span: Span, outer: Span) extends FofUnaryFormula {
  def children: Seq[Tree] = Vector(left, right)
  def withOuter(outer: Span): Formula = copy(outer = outer)
}

final case class QuantifiedFormula(q: Op[Quantifier], vars: Seq[NameNode], body: Formula, span: Span, outer: Span)
    extends FofUnitaryFormula {
  def children: Seq[Tree] = (q +: vars) :+ body
  def withOuter(outer: Span): Formula = copy(outer = outer)
}

final case class UnaryFormula(c: Op[UnaryConnective], body: Formula, span: Span, outer: Span) extends FofUnaryFormula {
  def children: Seq[Tree] = Vector(c, body)
  def withOuter(outer: Span): Formula = copy(outer = outer)
}

/** Also the associative `&` and `|` lists, nested to the left as in Leo. */
final case class BinaryFormula(c: Op[BinaryConnective], left: Formula, right: Formula, span: Span, outer: Span)
    extends FofBinaryFormula {
  def children: Seq[Tree] = Vector(c, left, right)
  def withOuter(outer: Span): Formula = copy(outer = outer)
}

/** A FOF term. Terms cannot be parenthesised, so they have no `outer`. */
sealed trait Term extends Tree

sealed trait FofTerm extends Term
sealed trait FofFunctionTerm extends FofTerm
sealed trait FofDefinedTerm extends FofFunctionTerm

/** `c`, `f(a)`, `$sum(a, b)`, `$$f(a)`. */
final case class AtomicTerm(f: Name, args: Seq[Term], span: Span) extends FofFunctionTerm {
  def kind: AtomKind = AtomKind.of(f.text)
  def children: Seq[Tree] = f +: args
}

final case class Variable(name: Name, span: Span) extends FofTerm {
  def children: Seq[Tree] = Vector(name)
}

final case class DistinctObject(text: String, span: Span) extends FofDefinedTerm {
  def children: Seq[Tree] = Nil
}

final case class NumberTerm(text: String, span: Span) extends FofDefinedTerm {
  def children: Seq[Tree] = Nil
}

/** A formula, term or name that is required but missing or unparsable. What it stands for is in the cause
  * (`MissingElement(Element.Term)`, `Unparsable(at)`, `IncompleteRule`, …); `span` covers whatever text it
  * stands for. `outer` matters only in formula positions; elsewhere it equals `span`. */
final case class ErrorNode(cause: ErrorCause, message: String, span: Span, outer: Span)
    extends Formula, Term, NameNode {
  def children: Seq[Tree] = Nil
  def withOuter(outer: Span): Formula = copy(outer = outer)
}

/** A parsed document. `errors` are lexer errors, which belong to no record. */
final case class TptpFile(records: Vector[Record], errors: Vector[SyntaxError], lines: LineIndex) {
  def allErrors: Vector[SyntaxError] = errors ++ records.flatMap(_.errors)

  /** Nodes from a record down to the innermost node at `offset`; empty if none. A node containing the offset is
    * preferred over one only touching it (a cursor right after a name). */
  def pathAt(offset: Int): List[Tree] = {
    def pick(ts: Seq[Tree]): Option[Tree] = ts.find(_.span.contains(offset)).orElse(ts.find(_.span.touches(offset)))
    def descend(t: Tree): List[Tree] = t :: pick(t.children).map(descend).getOrElse(Nil)
    pick(records).map(descend).getOrElse(Nil)
  }
}

enum Language { case Thf, Tff, Tcf, Cnf, Tpi }

// Every part the grammar requires is a node (the real one, or an ErrorNode in its place); Option only where the
// grammar allows absence.

sealed trait Record extends Tree {
  /** The record keyword (`fof`, `include`, …). */
  def keyword: Span
  /** Every syntax error of this record, including those without an error node. */
  def errors: Vector[SyntaxError]
}

/** `axiom`, or `axiom-<general_term>` with the part after `-` as `detail`. */
final case class Role(word: NameNode, detail: Option[Span], span: Span) extends Tree {
  def children: Seq[Tree] = Vector(word)
}

final case class FofRecord(keyword: Span, name: NameNode, role: Role, statement: Statement,
                           annotations: Option[Span], errors: Vector[SyntaxError], span: Span) extends Record {
  def children: Seq[Tree] = Vector(name, role, statement)
}

/** `selection` is `None` when everything is included (no selection, or `*`). */
final case class Include(keyword: Span, fileName: NameNode, selection: Option[Seq[NameNode]], space: Option[NameNode],
                         errors: Vector[SyntaxError], span: Span) extends Record {
  def children: Seq[Tree] = (fileName +: selection.getOrElse(Nil)) ++ space
}

/** A thf, tff, tcf, cnf or tpi record; its formula is not lowered. */
final case class OtherRecord(keyword: Span, language: Language, name: NameNode, role: Role,
                             errors: Vector[SyntaxError], span: Span) extends Record {
  def children: Seq[Tree] = Vector(name, role)
}

/** Text that is no record at all, e.g. tokens before the first record. */
final case class ErrorRecord(cause: ErrorCause, message: String, errors: Vector[SyntaxError], span: Span) extends Record {
  def keyword: Span = Span.empty(span.start)
  def children: Seq[Tree] = Nil
}
