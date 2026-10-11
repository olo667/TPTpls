package tptp.syntax.tree

import org.antlr.v4.runtime.{ParserRuleContext, Token as AntlrToken}
import org.antlr.v4.runtime.tree.TerminalNode
import scala.jdk.CollectionConverters.*
import tptp.syntax.{AntlrTokens, Element, ErrorCause, ErrorCauseStrategy, RepairToken, RuleKind, Span, SyntaxError}
import tptp.syntax.generated.TPTPParser.*

/** Turns one record's ANTLR parse tree into a compact `Record`. Holds on to nothing from the parse tree:
  * names copy their text, spans are plain values. Every part the grammar requires becomes a node; a part that
  * is a repair placeholder, ended with a recognition error, or is missing (`null` after default recovery)
  * becomes an `ErrorNode`. */
object Lowering {

  def record(ctx: Tptp_inputContext, errors: Vector[SyntaxError], trailing: Option[Span]): Record = {
    val sp = trailing.fold(span(ctx))(span(ctx).cover)
    if (ctx.include != null) include(ctx.include, errors, sp)
    else Option(ctx.annotated_formula).flatMap(a => Option(a.getChild(0))).collect { case c: ParserRuleContext => c } match {
      case Some(c: Fof_annotatedContext) =>
        FofRecord(AntlrTokens.span(c.getStart), nameNode(c.name, c, Element.Name), role(c.formula_role, c),
          statement(c.fof_formula, c), annotations(c.annotations), errors, sp)
      case Some(c: Thf_annotatedContext) => other(c, Language.Thf, c.name, c.formula_role, errors, sp)
      case Some(c: Tff_annotatedContext) => other(c, Language.Tff, c.name, c.formula_role, errors, sp)
      case Some(c: Tcf_annotatedContext) => other(c, Language.Tcf, c.name, c.formula_role, errors, sp)
      case Some(c: Cnf_annotatedContext) => other(c, Language.Cnf, c.name, c.formula_role, errors, sp)
      case Some(c: Tpi_annotatedContext) => other(c, Language.Tpi, c.name, c.formula_role, errors, sp)
      case _ => ErrorRecord(causeOf(ctx), "expected a record such as fof(...) or include(...)", errors, sp)
    }
  }

  /** The element of the outermost rule, from `ctx` upwards, that consists of `hole` alone. */
  def holeElement(ctx: ParserRuleContext, hole: AntlrToken): Element = {
    var outer = ctx
    var up = ctx.getParent
    while (up != null && (up.getStart eq hole) && (up.getStop eq hole)) { outer = up; up = up.getParent }
    Element.forRule(RuleKind(outer.getRuleIndex))
  }

  private def other(c: ParserRuleContext, language: Language, name: NameContext, r: Formula_roleContext,
                    errors: Vector[SyntaxError], sp: Span): Record =
    OtherRecord(AntlrTokens.span(c.getStart), language, nameNode(name, c, Element.Name), role(r, c), errors, sp)

  private def include(c: IncludeContext, errors: Vector[SyntaxError], sp: Span): Record = {
    val options = Option(c.include_optionals)
    val selection = options.flatMap(o => Option(o.formula_selection)).flatMap(s => Option(s.name_list)).map(names)
    val space = options.flatMap(o => Option(o.space_name)).map(s => nameNode(s.name, s, Element.Name))
    Include(AntlrTokens.span(c.getStart), nameNode(c.file_name, c, Element.FileName), selection, space, errors, sp)
  }

  private def names(list: Name_listContext): Vector[NameNode] = {
    val out = Vector.newBuilder[NameNode]
    var l = list
    while (l != null) { out += nameNode(l.name, l, Element.Name); l = l.name_list }
    out.result()
  }

  private def role(c: Formula_roleContext, parent: ParserRuleContext): Role =
    if (c == null) {
      val gap = missing(parent, Element.Role)
      Role(gap, None, gap.span)
    } else {
      val word = gap(c).map(errorNode).getOrElse {
        val t = Option(c.Lower_word).map(_.getSymbol)
        t.filter(usable).map(t => Name(t.getText, AntlrTokens.span(t))).getOrElse(incomplete(c, Element.Role))
      }
      Role(word, Option(c.general_term).map(span), span(c))
    }

  private def annotations(c: AnnotationsContext): Option[Span] =
    if (c == null || c.nothing != null) None else Some(span(c))

  /** A name-like rule (`name`, `file_name`, `variable`, …): its single word, or an `ErrorNode`. */
  private def nameNode(c: ParserRuleContext, parent: ParserRuleContext, what: Element): NameNode =
    if (c == null) missing(parent, what)
    else gap(c).map[NameNode](errorNode).orElse(word(c)).getOrElse(incomplete(c, what))

  /** The single word of a name-like rule, unless it is a repair token, a token ANTLR conjured, or the rule
    * ended with an error. */
  private def word(c: ParserRuleContext): Option[Name] =
    if (c == null || c.exception != null) None
    else Option(c.getStart).filter(t => (t eq c.getStop) && usable(t)).map(t => Name(t.getText, AntlrTokens.span(t)))

  private def usable(t: AntlrToken): Boolean =
    !t.isInstanceOf[RepairToken] && t.getType != AntlrToken.EOF && t.getTokenIndex >= 0

  // ---- statements and formulas ----

  private def statement(c: Fof_formulaContext, parent: ParserRuleContext): Statement =
    if (c == null) {
      val gap = missing(parent, Element.Formula)
      Logical(gap, gap.span)
    } else if (c.fof_sequent != null) sequent(c.fof_sequent)
    else {
      val f = formula(c)
      Logical(f, f.outer)
    }

  private def sequent(c: Fof_sequentContext): Statement =
    if (c.fof_sequent != null) sequent(c.fof_sequent)
    else Sequent(tuple(c.fof_formula_tuple(0)), tuple(c.fof_formula_tuple(1)), span(c))

  private def tuple(c: Fof_formula_tupleContext): Vector[Formula] =
    if (c == null || c.fof_formula_tuple_list == null) Vector.empty
    else {
      val l = c.fof_formula_tuple_list
      (l.fof_logic_formula +: l.comma_fof_logic_formula.asScala.toVector.map(_.fof_logic_formula)).map(sub(_, l))
    }

  /** Lowers a formula child of `parent`; a missing child becomes an error at the end of the parent. */
  private def sub(c: ParserRuleContext, parent: ParserRuleContext): Formula =
    if (c == null) missing(parent, Element.Formula) else formula(c)

  private def formula(ctx: ParserRuleContext): Formula =
    gap(ctx).map(errorNode).getOrElse {
      if (ctx.exception != null) incomplete(ctx, Element.Formula)
      else {
        val sp = span(ctx)
        ctx match {
          case c: Fof_unitary_formulaContext if c.fof_logic_formula != null => sub(c.fof_logic_formula, c).withOuter(sp)
          case c: Fof_binary_pairContext =>
            val connective = Option(c.nonassoc_connective).flatMap(n => BinaryConnective.fromText(n.getText).map(Op(_, span(n))))
            connective.fold(incomplete(c, Element.Formula))(op =>
              BinaryFormula(op, sub(c.fof_unit_formula(0), c), sub(c.fof_unit_formula(1), c), sp, sp))
          case c: Fof_or_formulaContext  => list(c, Option(c.fof_or_formula), c.fof_unit_formula.asScala.toVector, BinaryConnective.Or)
          case c: Fof_and_formulaContext => list(c, Option(c.fof_and_formula), c.fof_unit_formula.asScala.toVector, BinaryConnective.And)
          case c: Fof_unary_formulaContext if c.unary_connective != null =>
            UnaryFormula(Op(UnaryConnective.Not, span(c.unary_connective)), sub(c.fof_unit_formula, c), sp, sp)
          case c: Fof_infix_unaryContext           => Inequality(term(c.fof_term(0), c), term(c.fof_term(1), c), sp, sp)
          case c: Fof_defined_infix_formulaContext => Equality(term(c.fof_term(0), c), term(c.fof_term(1), c), sp, sp)
          case c: Fof_quantified_formulaContext if c.fof_quantifier == null => incomplete(c, Element.Formula)
          case c: Fof_quantified_formulaContext =>
            val q = if (c.fof_quantifier.getText == "!") Quantifier.ForAll else Quantifier.Exists
            QuantifiedFormula(Op(q, span(c.fof_quantifier)), variables(c.fof_variable_list, c), sub(c.fof_unit_formula, c), sp, sp)
          case c @ (_: Fof_plain_termContext | _: Fof_defined_plain_termContext | _: Fof_system_termContext) =>
            atom(c).fold(incomplete(c, Element.Formula)) { case (f, args) => AtomicFormula(f, args, sp, sp) }
          // chain rules, and the parts of rules above whose only rule child is the formula
          case c => firstRule(c).fold(incomplete(c, Element.Formula))(formula)
        }
      }
    }

  /** `a | b | c`: the grammar nests to the left (`or_formula | unit`), and so does the tree. */
  private def list(c: ParserRuleContext, left: Option[ParserRuleContext], units: Vector[Fof_unit_formulaContext],
                   connective: BinaryConnective): Formula = {
    val op = c.children.asScala.collectFirst { case t: TerminalNode => Op(connective, AntlrTokens.span(t.getSymbol)) }
    val (l, r) = left match {
      case Some(nested) => (formula(nested), units.headOption)
      case None         => (sub(units.headOption.orNull, c), units.lift(1))
    }
    op.fold(incomplete(c, Element.Formula))(BinaryFormula(_, l, sub(r.orNull, c), span(c), span(c)))
  }

  private def variables(c: Fof_variable_listContext, parent: ParserRuleContext): Vector[NameNode] =
    if (c == null) Vector(missing(parent, Element.Variable))
    else {
      val out = Vector.newBuilder[NameNode]
      var l = c
      while (l != null) { out += nameNode(l.variable, l, Element.Variable); l = l.fof_variable_list }
      out.result()
    }

  // ---- terms ----

  private def term(c: ParserRuleContext, parent: ParserRuleContext): Term =
    if (c == null) missing(parent, Element.Term)
    else
      gap(c).map(errorNode).getOrElse {
        if (c.exception != null) incomplete(c, Element.Term)
        else
          c match {
            case v: VariableContext => word(v).fold(incomplete(v, Element.Term))(n => Variable(n, n.span))
            case d: Defined_termContext =>
              if (d.Distinct_object != null) DistinctObject(d.Distinct_object.getText, span(d))
              else NumberTerm(d.getText, span(d))
            case a @ (_: Fof_plain_termContext | _: Fof_defined_plain_termContext | _: Fof_system_termContext) =>
              atom(a).fold(incomplete(a, Element.Term)) { case (f, args) => AtomicTerm(f, args, span(a)) }
            case other => firstRule(other).fold(incomplete(other, Element.Term))(term(_, other))
          }
      }

  /** Functor and arguments of a plain, defined or system term (also used for atoms). */
  private def atom(c: ParserRuleContext): Option[(Name, Vector[Term])] = {
    val (functor, args) = c match {
      case p: Fof_plain_termContext         => (Option[ParserRuleContext](p.constant).getOrElse(p.functor), p.fof_arguments)
      case d: Fof_defined_plain_termContext => (Option[ParserRuleContext](d.defined_constant).getOrElse(d.defined_functor), d.fof_arguments)
      case s: Fof_system_termContext        => (Option[ParserRuleContext](s.system_constant).getOrElse(s.system_functor), s.fof_arguments)
      case _                                => (null, null)
    }
    word(functor).map(f => (f, arguments(args)))
  }

  private def arguments(c: Fof_argumentsContext): Vector[Term] = {
    val out = Vector.newBuilder[Term]
    var a = c
    while (a != null) { out += term(a.fof_term, a); a = a.fof_arguments }
    out.result()
  }

  // ---- gaps and errors ----

  /** A rule that consists of one repair placeholder: a hole (missing element) or a collapsed bracket group. */
  private def gap(c: ParserRuleContext): Option[(ErrorCause, String, Span)] =
    c.getStart match {
      case r: RepairToken if (r eq c.getStop) && r.origin == RepairToken.Origin.Hole =>
        val what = holeElement(c, r)
        Some((ErrorCause.MissingElement(what), s"missing ${what.label}", AntlrTokens.span(r)))
      case r: RepairToken if (r eq c.getStop) && r.origin == RepairToken.Origin.Collapsed =>
        Some((ErrorCause.Unparsable(r.failedAt), "could not parse this part", tokenSpan(r)))
      case _ => None
    }

  private def errorNode(g: (ErrorCause, String, Span)): ErrorNode = ErrorNode(g._1, g._2, g._3, g._3)

  /** A rule that ended with a recognition error, covering what it consumed. */
  private def incomplete(c: ParserRuleContext, what: Element): ErrorNode = {
    val s = span(c)
    ErrorNode(causeOf(c), s"incomplete ${what.label}", s, s)
  }

  /** A required rule that default recovery never reached: zero width at the end of its parent. */
  private def missing(parent: ParserRuleContext, what: Element): ErrorNode = {
    val s = Span.empty(span(parent).end)
    ErrorNode(ErrorCause.IncompleteRule, s"missing ${what.label}", s, s)
  }

  private def causeOf(c: ParserRuleContext): ErrorCause =
    Option(c.exception).map(ErrorCauseStrategy.causeOf).getOrElse(ErrorCause.IncompleteRule)

  private def firstRule(c: ParserRuleContext): Option[ParserRuleContext] =
    Option(c.children).flatMap(_.asScala.collectFirst { case p: ParserRuleContext => p })

  // ---- spans ----

  private def tokenSpan(t: AntlrToken): Span = t match {
    case r: RepairToken if r.origin == RepairToken.Origin.Collapsed && r.collapsed.nonEmpty =>
      AntlrTokens.span(r.collapsed.head).cover(AntlrTokens.span(r.collapsed.last))
    case _ => AntlrTokens.span(t)
  }

  private def span(c: ParserRuleContext): Span = {
    val start = tokenSpan(c.getStart)
    val stop = c.getStop
    if (stop == null || stop.getTokenIndex < c.getStart.getTokenIndex) Span.empty(start.start)
    else start.cover(tokenSpan(stop))
  }
}
