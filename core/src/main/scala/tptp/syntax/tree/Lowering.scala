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
      case Some(c: Tff_annotatedContext) =>
        TffRecord(AntlrTokens.span(c.getStart), nameNode(c.name, c, Element.Name), role(c.formula_role, c),
          tffStatement(c.tff_formula, c), annotations(c.annotations), errors, sp)
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
          case c: Tff_unitary_formulaContext if c.tff_logic_formula != null => sub(c.tff_logic_formula, c).withOuter(sp)
          case c: Tff_binary_pairContext =>
            val connective = Option(c.nonassoc_connective).flatMap(n => BinaryConnective.fromText(n.getText).map(Op(_, span(n))))
            connective.fold(incomplete(c, Element.Formula))(op =>
              BinaryFormula(op, sub(c.tff_unit_formula(0), c), sub(c.tff_unit_formula(1), c), sp, sp))
          case c: Tff_or_formulaContext  => list(c, Option(c.tff_or_formula), c.tff_unit_formula.asScala.toVector, BinaryConnective.Or)
          case c: Tff_and_formulaContext => list(c, Option(c.tff_and_formula), c.tff_unit_formula.asScala.toVector, BinaryConnective.And)
          case c: Tff_prefix_unaryContext if c.tff_unary_connective == null => incomplete(c, Element.Formula)
          case c: Tff_prefix_unaryContext =>
            if (c.tff_unary_connective.unary_connective == null) unsupported(c) // an NX0 connective
            else UnaryFormula(Op(UnaryConnective.Not, span(c.tff_unary_connective)), sub(c.tff_preunit_formula, c), sp, sp)
          case c: Tff_infix_unaryContext   => Inequality(tffTerm(c.tff_unitary_term(0), c), tffTerm(c.tff_unitary_term(1), c), sp, sp)
          case c: Tff_defined_infixContext => Equality(tffTerm(c.tff_unitary_term(0), c), tffTerm(c.tff_unitary_term(1), c), sp, sp)
          case c: Tff_quantified_formulaContext if c.tff_quantifier == null => incomplete(c, Element.Formula)
          case c: Tff_quantified_formulaContext =>
            if (c.tff_quantifier.fof_quantifier == null) unsupported(c) // `#`
            else {
              val q = if (c.tff_quantifier.getText == "!") Quantifier.ForAll else Quantifier.Exists
              QuantifiedFormula(Op(q, span(c.tff_quantifier)), bindings(c.tff_variable_list, c), sub(c.tff_unit_formula, c), sp, sp)
            }
          case c: Tff_defined_plainContext if c.txf_let != null  => let(c.txf_let)
          case c: Tff_defined_plainContext if c.nxf_atom != null => unsupported(c)
          case c @ (_: Tff_plain_atomicContext | _: Tff_defined_plainContext | _: Tff_system_atomicContext) =>
            tffAtom(c).fold(incomplete(c, Element.Formula)) { case (f, args) => AtomicFormula(f, args, sp, sp) }
          case c: Txf_unitary_formulaContext =>
            word(c.variable).fold[Formula](incomplete(c, Element.Formula))(n => VariableFormula(n, sp, sp))
          case c @ (_: Txf_definitionContext | _: Txf_sequentContext) => unsupported(c) // nested; at the top: tffStatement
          // chain rules, and the parts of rules above whose only rule child is the formula
          case c => firstRule(c).fold(incomplete(c, Element.Formula))(formula)
        }
      }
    }

  /** `a | b | c`: the grammar nests to the left (`or_formula | unit`), and so does the tree. */
  private def list(c: ParserRuleContext, left: Option[ParserRuleContext], units: Vector[ParserRuleContext],
                   connective: BinaryConnective): Formula = {
    val op = c.children.asScala.collectFirst { case t: TerminalNode => Op(connective, AntlrTokens.span(t.getSymbol)) }
    val (l, r) = left match {
      case Some(nested) => (formula(nested), units.headOption)
      case None         => (sub(units.headOption.orNull, c), units.lift(1))
    }
    op.fold(incomplete(c, Element.Formula))(BinaryFormula(_, l, sub(r.orNull, c), span(c), span(c)))
  }

  private def variables(c: Fof_variable_listContext, parent: ParserRuleContext): Vector[VarBinding] =
    if (c == null) Vector(untyped(missing(parent, Element.Variable)))
    else {
      val out = Vector.newBuilder[VarBinding]
      var l = c
      while (l != null) { out += untyped(nameNode(l.variable, l, Element.Variable)); l = l.fof_variable_list }
      out.result()
    }

  private def untyped(n: NameNode): VarBinding = VarBinding(n, None, n.span)

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

  // ---- TFF ----

  private def tffStatement(c: Tff_formulaContext, parent: ParserRuleContext): Statement =
    if (c == null) {
      val gap = missing(parent, Element.Formula)
      Logical(gap, gap.span)
    } else if (gap(c).isEmpty && c.exception == null && c.tff_atom_typing != null) typing(c.tff_atom_typing)
    else if (gap(c).isEmpty && c.exception == null && c.tff_subtype != null) {
      val st = c.tff_subtype
      Subtype(tffType(st.atomic_type(0), st), tffType(st.atomic_type(1), st), span(st))
    } else
      txfTop(c) match {
        case Some(d: Txf_definitionContext) => Definition(tffTerm(d.tff_atomic_formula, d), tffTerm(d.tff_term, d), span(d))
        case Some(s: Txf_sequentContext) => Sequent(tupleFormulas(s.txf_tuple(0), s), tupleFormulas(s.txf_tuple(1), s), span(s))
        case _ =>
          val f = formula(c)
          Logical(f, f.outer)
      }

  /** The definition or sequent a TFF formula consists of, found through single-child chain rules. */
  private def txfTop(c: ParserRuleContext): Option[ParserRuleContext] = c match {
    case d: Txf_definitionContext => Some(d)
    case s: Txf_sequentContext    => Some(s)
    case other if gap(other).isEmpty && other.exception == null && chain(other) => firstRule(other).flatMap(txfTop)
    case _                        => None
  }

  private def tupleFormulas(t: Txf_tupleContext, parent: ParserRuleContext): Vector[Formula] =
    if (t == null) Vector(missing(parent, Element.Formula))
    else Option(t.tff_arguments).toVector.flatMap(a => a.tff_term +: a.comma_tff_term.asScala.toVector.map(_.tff_term)).map(sub(_, t))

  private def let(c: Txf_letContext): Let = {
    val sp = span(c)
    val typings = Option(c.txf_let_types).toVector.flatMap { t =>
      if (t.tff_atom_typing != null) Vector(t.tff_atom_typing)
      else {
        val out = Vector.newBuilder[Tff_atom_typingContext]
        var l = t.tff_atom_typing_list
        while (l != null) { out += l.tff_atom_typing; l = l.tff_atom_typing_list }
        out.result()
      }
    }.filter(_ != null).map(typing).collect { case d: TypeDeclaration => d }
    val definitions = Option(c.txf_let_defns).toVector.flatMap { d =>
      if (d.txf_let_defn != null) Vector(d.txf_let_defn)
      else {
        val out = Vector.newBuilder[Txf_let_defnContext]
        var l = d.txf_let_defn_list
        while (l != null) { out += l.txf_let_defn; l = l.txf_let_defn_list }
        out.result()
      }
    }.filter(_ != null).map { d =>
      val lhs = Option(d.txf_let_lhs).map(l => tffTerm(Option[ParserRuleContext](l.txf_tuple).getOrElse(l.tff_plain_atomic), l))
        .getOrElse(missing(d, Element.Term))
      LetDefinition(lhs, tffTerm(d.tff_term, d), span(d))
    }
    Let(typings, definitions, tffTerm(c.tff_term, c), sp, sp)
  }

  private def typing(c: Tff_atom_typingContext): Statement =
    if (c.tff_atom_typing != null) typing(c.tff_atom_typing)
    else if (c.typeable_atom != null && c.typeable_atom.Distinct_object != null) { val u = unsupported(c); Logical(u, u.span) }
    else TypeDeclaration(nameNode(Option(c.typeable_atom).map(_.constant).orNull, c, Element.Name),
      tffType(c.tff_top_level_type, c), span(c))

  private def bindings(c: Tff_variable_listContext, parent: ParserRuleContext): Vector[VarBinding] =
    if (c == null) Vector(untyped(missing(parent, Element.Variable)))
    else {
      val out = Vector.newBuilder[VarBinding]
      var l = c
      while (l != null) { out += binding(l.tff_variable, l); l = l.tff_variable_list }
      out.result()
    }

  private def binding(c: Tff_variableContext, parent: ParserRuleContext): VarBinding =
    if (c == null) untyped(missing(parent, Element.Variable))
    else gap(c).map(g => untyped(errorNode(g))).getOrElse {
      val t = c.tff_typed_variable
      if (t == null) untyped(nameNode(c.variable, c, Element.Variable))
      else VarBinding(nameNode(t.variable, t, Element.Variable), Some(tffType(t.tff_atomic_type, t)), span(t))
    }

  /** A term position: an atom is a term, a variable a variable, a tuple a tuple; any other formula is a TXF formula
    * as a term. */
  private def tffTerm(c: ParserRuleContext, parent: ParserRuleContext): Term =
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
            case d: Tff_defined_plainContext if d.txf_let != null  => let(d.txf_let)
            case d: Tff_defined_plainContext if d.nxf_atom != null => unsupported(d)
            case t: Txf_tupleContext => Tuple(tffArguments(t.tff_arguments), span(t))
            case a @ (_: Tff_plain_atomicContext | _: Tff_defined_plainContext | _: Tff_system_atomicContext) =>
              tffAtom(a).fold(incomplete(a, Element.Term)) { case (f, args) => AtomicTerm(f, args, span(a)) }
            case other if chain(other) => firstRule(other).fold(incomplete(other, Element.Term))(tffTerm(_, other))
            case other                 => FormulaTerm(formula(other), span(other)) // a TXF formula as a term
          }
      }

  /** A rule with a single rule child: a chain step on the way to an atom, variable or defined term. */
  private def chain(c: ParserRuleContext): Boolean = c.getChildCount == 1 && c.getChild(0).isInstanceOf[ParserRuleContext]

  private def tffAtom(c: ParserRuleContext): Option[(Name, Vector[Term])] = {
    val (functor, args) = c match {
      case p: Tff_plain_atomicContext  => (Option[ParserRuleContext](p.constant).getOrElse(p.functor), p.tff_arguments)
      case d: Tff_defined_plainContext => (Option[ParserRuleContext](d.defined_constant).getOrElse(d.defined_functor), d.tff_arguments)
      case s: Tff_system_atomicContext => (Option[ParserRuleContext](s.system_constant).getOrElse(s.system_functor), s.tff_arguments)
      case _                           => (null, null)
    }
    word(functor).map(f => (f, tffArguments(args)))
  }

  private def tffArguments(c: Tff_argumentsContext): Vector[Term] =
    if (c == null) Vector.empty
    else tffTerm(c.tff_term, c) +: c.comma_tff_term.asScala.toVector.map(t => tffTerm(t.tff_term, t))

  private def tffType(c: ParserRuleContext, parent: ParserRuleContext): Type =
    if (c == null) missing(parent, Element.Type)
    else
      gap(c).map(errorNode).getOrElse {
        if (c.exception != null) incomplete(c, Element.Type)
        else {
          val sp = span(c)
          c match {
            case t: Tff_atomic_typeContext =>
              if (t.txf_tuple_type != null) TupleType(typeList(t.txf_tuple_type.tff_type_list, t.txf_tuple_type), sp)
              else if (t.variable != null) word(t.variable).fold[Type](incomplete(t, Element.Type))(TypeVariable(_, sp))
              else if (t.type_functor != null)
                word(t.type_functor).fold[Type](incomplete(t, Element.Type))(f => TypeApp(f, typeArguments(t.tff_type_arguments, t), sp))
              else if (t.tff_atomic_type != null) tffType(t.tff_atomic_type, t) // parenthesised
              else firstRule(t).flatMap(word).fold[Type](incomplete(t, Element.Type))(TypeApp(_, Vector.empty, sp)) // constant, defined type
            case a: Atomic_typeContext => word(a).fold[Type](incomplete(a, Element.Type))(TypeApp(_, Vector.empty, sp)) // subtypes
            case m: Tff_mapping_typeContext => MappingType(product(m.tff_unitary_type, m), tffType(m.tff_atomic_type, m), sp)
            case q: Tf1_quantified_typeContext =>
              if (q.type_quantifier == null || q.type_quantifier.getText != "!>") unsupported(q) // `?*`
              else QuantifiedType(bindings(q.tff_variable_list, q), tffType(q.tff_monotype, q), sp)
            // top-level, non-atomic, monotype and unitary types: chain rules and parentheses
            case other => firstRule(other).fold[Type](incomplete(other, Element.Type))(tffType(_, other))
          }
        }
      }

  private def typeList(c: Tff_type_listContext, parent: ParserRuleContext): Vector[Type] =
    if (c == null) Vector(missing(parent, Element.Type))
    else {
      val out = Vector.newBuilder[Type]
      var l = c
      while (l != null) { out += tffType(l.tff_top_level_type, l); l = l.tff_type_list }
      out.result()
    }

  private def typeArguments(c: Tff_type_argumentsContext, parent: ParserRuleContext): Vector[Type] =
    if (c == null) Vector(missing(parent, Element.Type))
    else {
      val out = Vector.newBuilder[Type]
      var a = c
      while (a != null) { out += tffType(a.tff_atomic_type, a); a = a.tff_type_arguments }
      out.result()
    }

  /** The argument types of a mapping: one unitary type, or a parenthesised `*` chain, flattened. */
  private def product(c: Tff_unitary_typeContext, parent: ParserRuleContext): Vector[Type] =
    if (c == null || gap(c).isDefined || c.exception != null || c.tff_xprod_type == null) Vector(tffType(c, parent))
    else xprod(c.tff_xprod_type)

  private def xprod(c: Tff_xprod_typeContext): Vector[Type] =
    if (c.tff_xprod_type != null) xprod(c.tff_xprod_type) :+ tffType(c.tff_atomic_type, c)
    else product(c.tff_unitary_type, c) :+ tffType(c.tff_atomic_type, c)

  private def unsupported(c: ParserRuleContext): Unsupported = {
    val s = span(c)
    Unsupported(RuleKind(c.getRuleIndex).name, s, s)
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
