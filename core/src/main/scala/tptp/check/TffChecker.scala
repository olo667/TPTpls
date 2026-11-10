package tptp.check

import tptp.semantics.{Diagnostic, DiagnosticCode, Location, RelatedInfo, Severity}
import tptp.syntax.Span
import tptp.syntax.languages.{Fof, Tff}
import tptp.syntax.tree.*

/** Checks TFF records: `local` needs only the record (roles, `$`-words, variables, the declaration summary);
  * `typed` checks types against the declarations visible to the record. */
object TffChecker {
  /** A scope key no variable can have: marks a scope below a quantifier with a broken variable list. */
  private val Opaque = "?opaque"

  /** In term positions any defined word may stand: functors, Boolean values (TXF), and defined types as TF1 type
    * arguments. */
  private val termWords: Set[String] =
    Tff.definedFunctors ++ Tff.definedPredicates ++ Tff.definedPropositions ++ Tff.definedTypes.keySet

  def local(r: TffRecord): RecordCheck[SummaryItem] = {
    val walk = new LocalWalk(r.errors.map(_.span), checkWords = !unchecked(r))
    role(r).foreach(walk.diagnostics += _)
    val formula = SummaryItem.Formula(r.name.valid, Vector.empty)
    val item = r.statement match {
      case TypeDeclaration(symbol, ty, _) =>
        walk.declaredType(ty)
        symbol.valid.map(s => SummaryItem.Declaration(r.name.valid, s, structural(ty, Set.empty))).getOrElse(formula)
      case Logical(f, _)         => walk.formula(f, Map.empty); formula
      case Sequent(l, rs, _)     => (l ++ rs).foreach(walk.formula(_, Map.empty)); formula
      case Definition(l, rhs, _) => walk.term(l, Map.empty); walk.term(rhs, Map.empty); formula
      case Subtype(sub, sup, _)  => walk.declaredType(sub); walk.declaredType(sup); formula
    }
    RecordCheck(item, walk.diagnostics.result())
  }

  /** Whether the record is not checked for types and `$`-words: an NX0 logic specification (role `logic`), or a record
    * containing a construct that is not checked (NX0). */
  def unchecked(r: TffRecord): Boolean = {
    def go(t: Tree): Boolean = t match {
      case _: Unsupported => true
      case other          => other.children.exists(go)
    }
    r.role.word.valid.exists(_.text == "logic") || go(r.statement)
  }

  /** A type as written, without looking anything up: plain names become constructors, bound type variables
    * variables; nested `!>` are merged; errors, unsupported parts and unbound variables are `Unknown`. */
  def structural(t: Type, bound: Set[String]): Ty = t match {
    case TypeApp(f, args, _) =>
      Tff.definedTypes.get(f.text) match {
        case Some("$tType") => Ty.Kind
        case Some(name)     => Ty.Con(name, Vector.empty)
        case None           => Ty.Con(f.key, args.toVector.map(structural(_, bound)))
      }
    case TypeVariable(n, _)        => if (bound(n.key)) Ty.Var(n.key) else Ty.Unknown
    case MappingType(args, res, _) => Ty.Fun(args.toVector.map(structural(_, bound)), structural(res, bound))
    case TupleType(es, _)          => Ty.Tuple(es.toVector.map(structural(_, bound)))
    case QuantifiedType(vars, body, _) =>
      val names = vars.flatMap(_.name.valid).map(_.key).toVector
      Ty.forall(names, structural(body, bound ++ names))
    case _: ErrorNode | _: Unsupported => Ty.Unknown
  }

  /** Checks a record's types against `sig`. Unchecked records (see `unchecked`) get nothing. In an axiom file opened on
    * its own (`axiomFile`), undeclared types and symbols are warnings and their values unknown. `locate` turns a span
    * of the record's file into a location. */
  def typed(r: TffRecord, sig: Signature, axiomFile: Boolean, locate: Span => Location): Vector[TypeProblem] =
    if (unchecked(r)) Vector.empty
    else {
      val walk = new TypedWalk(sig, axiomFile, r.errors.map(_.span), locate)
      r.statement match {
        case TypeDeclaration(_, ty, _) => walk.checkType(ty, Map.empty)
        case Logical(f, _)             => walk.formula(f, Map.empty)
        case Sequent(l, rs, _)         => (l ++ rs).foreach(walk.formula(_, Map.empty))
        case Definition(l, rhs, _)     => walk.definition(l, rhs, Map.empty)
        case Subtype(sub, sup, _)      => walk.checkType(sub, Map.empty); walk.checkType(sup, Map.empty)
      }
      walk.problems.result()
    }

  /** Variables in scope: a term variable's type, or `Kind` for a type variable. */
  private type Scope = Map[String, Ty]

  private final class TypedWalk(sig: Signature, axiomFile: Boolean, errors: Vector[Span], locate: Span => Location) {
    val problems = Vector.newBuilder[TypeProblem]
    private val warnedUndeclared = scala.collection.mutable.Set.empty[String]
    /** `$let`-local declarations, shadowing the signature. */
    private var local = Map.empty[String, Vector[Decl]]

    private def lookup(key: String): Vector[Decl] = local.getOrElse(key, sig(key))
    private def symbols(key: String): Vector[Decl] = lookup(key).filterNot(d => Ty.isType(d.ty))
    private def types(key: String): Vector[Decl] = lookup(key).filter(d => Ty.isType(d.ty))

    def formula(f: Formula, scope: Scope): Unit = f match {
      case a: AtomicFormula             => apply(a.f, a.args, a.span, scope, asFormula = true)
      case Equality(l, r, _, _)         => sides("=", l, r, scope)
      case Inequality(l, r, _, _)       => sides("!=", l, r, scope)
      case QuantifiedFormula(q, vars, body, _, _) => formula(body, bind(q.kind, vars, scope))
      case UnaryFormula(_, body, _, _)  => formula(body, scope)
      case BinaryFormula(_, l, r, _, _) => formula(l, scope); formula(r, scope)
      case VariableFormula(n, sp, _) =>
        val t = scope.getOrElse(n.key, Ty.Unknown)
        if (!Ty.compatible(t, Ty.Bool))
          report(n.text, s"`${Ty.show(t)}` as a formula", sp, DiagnosticCode.TypeMismatch,
            s"variable `${n.text}` has type `${Ty.show(t)}`, but is used as a formula")
      case l: Let =>
        val t = let(l, scope)
        if (!Ty.compatible(t, Ty.Bool))
          report("$let", s"`${Ty.show(t)}` as a formula", l.body.span, DiagnosticCode.TypeMismatch,
            s"this `$$let` has type `${Ty.show(t)}`, but is used as a formula")
      case _: ErrorNode | _: Unsupported => ()
    }

    def term(t: Term, scope: Scope): Ty = t match {
      case a: AtomicTerm       => apply(a.f, a.args, a.span, scope, asFormula = false)
      case Variable(n, _)      => scope.getOrElse(n.key, Ty.Unknown) // unbound: reported by the local checks
      case NumberTerm(text, _) => Ty.Con(numberType(text), Vector.empty)
      case _: DistinctObject   => Ty.Individual
      case FormulaTerm(f, _)   => formula(f, scope); Ty.Bool
      case Tuple(es, _)        => Ty.Tuple(es.toVector.map(term(_, scope)))
      case l: Let              => let(l, scope)
      case _: ErrorNode | _: Unsupported      => Ty.Unknown
    }

    def definition(l: Term, r: Term, scope: Scope): Unit = {
      val (lt, rt) = (term(l, scope), term(r, scope))
      if (!Ty.compatible(lt, rt))
        report("==", s"`${Ty.show(lt)}` vs `${Ty.show(rt)}`", r.span, DiagnosticCode.TypeMismatch,
          s"the sides of `==` have types `${Ty.show(lt)}` and `${Ty.show(rt)}`")
    }

    /** `$let`: its typings shadow the signature in its definitions and body; returns the body's type. */
    private def let(l: Let, scope: Scope): Ty = {
      val decls = l.typings.flatMap(t => t.symbol.valid.map(s => Decl(s, checkType(t.ty, scope), locate(s.span)))).toVector
      val outer = local
      local = local ++ decls.groupBy(_.symbol.key)
      try {
        l.definitions.foreach(d => letDefinition(d, decls.map(_.symbol.key).toSet, scope))
        term(l.body, scope)
      } finally local = outer
    }

    private def letDefinition(d: LetDefinition, typed: Set[String], scope: Scope): Unit = {
      def invalid(at: Span, message: String): Unit =
        report("$let", "invalid definition", at, DiagnosticCode.InvalidLetDefinition, message)
      val shape = "the left-hand side must be a symbol applied to variables, or a tuple of constants"
      def isTyped(n: Name): Boolean = typed(n.key) || { invalid(n.span, s"`${n.text}` is not typed in this `$$let`"); false }
      def check(want: Ty, got: Ty, what: String): Unit =
        if (!Ty.compatible(want, got))
          report(what, s"right-hand side: `${Ty.show(got)}` vs `${Ty.show(want)}`", d.rhs.span, DiagnosticCode.TypeMismatch,
            s"the right-hand side of `$what` has type `${Ty.show(got)}`, but `$what` has type `${Ty.show(want)}`")
      d.lhs match {
        case AtomicTerm(f, args, _) if AtomKind.of(f.text) == AtomKind.Plain =>
          val vars = args.collect { case Variable(n, _) => n }
          val repeated = vars.groupBy(_.key).values.filter(_.size > 1).map(_(1)).toVector.sortBy(_.span.start)
          if (vars.size != args.size) { invalid(d.lhs.span, shape); term(d.rhs, scope) }
          else if (repeated.nonEmpty) { repeated.foreach(n => invalid(n.span, s"`${n.text}` appears twice on the left-hand side")); term(d.rhs, scope) }
          else if (!isTyped(f)) term(d.rhs, scope)
          else {
            val decl = local(f.key).find(dd => Ty.arity(dd.ty).contains(args.size))
            val params = decl.map(_.ty) match {
              case Some(Ty.Fun(as, _)) => vars.map(_.key).zip(as)
              case _                   => vars.map(_.key -> Ty.Unknown)
            }
            check(decl.fold(Ty.Unknown)(dd => Ty.result(dd.ty)), term(d.rhs, scope ++ params), f.text)
          }
        case Tuple(es, _) if es.forall { case AtomicTerm(_, Seq(), _) => true; case _ => false } =>
          val names = es.collect { case AtomicTerm(n, _, _) => n }
          if (names.forall(isTyped)) {
            val want = Ty.Tuple(names.toVector.map(n => local(n.key).headOption.fold(Ty.Unknown)(_.ty)))
            val got = term(d.rhs, scope)
            if (!Ty.compatible(want, got))
              report("$let", s"`${Ty.show(got)}` vs `${Ty.show(want)}`", d.rhs.span, DiagnosticCode.TypeMismatch,
                s"the right-hand side has type `${Ty.show(got)}`, but the left-hand side has type `${Ty.show(want)}`")
          } else term(d.rhs, scope)
        case _: ErrorNode | _: Unsupported => term(d.rhs, scope)
        case other =>
          invalid(other.span, shape)
          term(d.rhs, scope)
      }
    }

    /** A variable list: each binding's type (default `$i`), checked; `$tType` binds a type variable, only under `!`. */
    private def bind(q: Quantifier, vars: Seq[VarBinding], scope: Scope): Scope = vars.foldLeft(scope) { (s, v) =>
      v.name.valid.fold(s) { n =>
        val ty = v.ty.fold(Ty.Individual)(t => checkType(t, s))
        if (ty == Ty.Kind && q == Quantifier.Exists)
          report(n.text, "type variable under `?`", v.span, DiagnosticCode.MalformedType, "type variables are bound by `!`, not `?`")
        s + (n.key -> ty)
      }
    }

    /** Checks a written type and returns it; undeclared types and wrong argument counts are reported and `Unknown`. */
    def checkType(t: Type, scope: Scope): Ty = t match {
      case TypeApp(f, args, sp) =>
        Tff.definedTypes.get(f.text) match {
          case Some("$tType") => Ty.Kind
          case Some(name)     => Ty.Con(name, Vector.empty)
          case None =>
            val argTys = args.toVector.map(checkType(_, scope))
            types(f.key) match {
              case Vector() => undeclaredType(f, sp); Ty.Unknown
              case decls if decls.exists(d => Ty.arity(d.ty).contains(args.size)) => Ty.Con(f.key, argTys)
              case decls =>
                val counts = decls.flatMap(d => Ty.arity(d.ty)).distinct.sorted
                report(f.text, s"${args.size} vs ${counts.mkString(" or ")} type arguments", sp, DiagnosticCode.WrongTypeArguments,
                  s"type `${f.text}` takes ${counts.map(plural(_, "argument")).mkString(" or ")}, but has ${args.size}")
                Ty.Unknown
            }
        }
      case TypeVariable(n, _) => if (scope.get(n.key).contains(Ty.Kind)) Ty.Var(n.key) else Ty.Unknown
      case MappingType(args, res, sp) =>
        val (as, r) = (args.toVector.map(checkType(_, scope)), checkType(res, scope))
        if ((as :+ r).filter(_ != Ty.Unknown).map(_ == Ty.Kind).distinct.size > 1)
          report("mapping", "mixes `$tType`", sp, DiagnosticCode.MalformedType, "a type constructor maps `$tType`s to `$tType`")
        Ty.Fun(as, r)
      case TupleType(es, _) => Ty.Tuple(es.toVector.map(checkType(_, scope)))
      case QuantifiedType(vars, body, _) =>
        vars.foreach { v =>
          val kind = v.ty.exists { case TypeApp(k, Seq(), _) => k.text == "$tType"; case _: ErrorNode => true; case _ => false }
          if (!kind) v.name.valid.foreach(n =>
            report(n.text, "type variable without `$tType`", v.span, DiagnosticCode.MalformedType,
              s"type variable `${n.text}` must have type `$$tType`"))
        }
        val names = vars.flatMap(_.name.valid).map(_.key).toVector
        Ty.forall(names, checkType(body, scope ++ names.map(_ -> Ty.Kind)))
      case _: ErrorNode | _: Unsupported => Ty.Unknown
    }

    private def undeclaredType(f: Name, at: Span): Unit =
      if (axiomFile)
        report(f.text, "undeclared type", at, DiagnosticCode.UndeclaredType,
          s"type `${f.text}` is not declared in this file or its includes", Severity.Warning)
      else report(f.text, "undeclared type", at, DiagnosticCode.UndeclaredType, s"type `${f.text}` is not declared")

    /** An atom or function application; returns its type (`Unknown` where it cannot be known). */
    private def apply(f: Name, args: Seq[Term], span: Span, scope: Scope, asFormula: Boolean): Ty = {
      val damaged = args.exists(_.isInstanceOf[ErrorNode]) || droppedToken(span, args)
      AtomKind.of(f.text) match {
        case AtomKind.Defined =>
          val result =
            if (Tff.definedPropositions(f.text) && args.isEmpty) Ty.Bool
            else if (damaged) { args.foreach(term(_, scope)); Ty.Unknown }
            else defined(f, args, scope)
          position(f, result, asFormula, None)
        case _ =>
          val decls = symbols(f.key)
          if (damaged) { args.foreach(term(_, scope)); Ty.Unknown }
          else if (decls.isEmpty && types(f.key).nonEmpty) {
            args.foreach(term(_, scope))
            report(f.text, "a type used as a term", f.span, DiagnosticCode.TypeMismatch,
              s"`${f.text}` is a type, but used as ${if (asFormula) "a formula" else "a term"} here", related = types(f.key).headOption)
            Ty.Unknown
          } else if (decls.isEmpty) undeclared(f, args, scope, asFormula)
          else
            decls.find(d => Ty.arity(d.ty).contains(args.size)) match {
              case None =>
                args.foreach(term(_, scope))
                val counts = decls.flatMap(d => Ty.arity(d.ty)).distinct.sorted
                decls.collectFirst { case d @ Decl(_, Ty.Forall(vs, body), _) if Ty.arity(body).contains(args.size) => (d, vs.size) } match {
                  case Some((d, k)) =>
                    report(f.text, s"0 vs $k type arguments", span, DiagnosticCode.WrongTypeArguments,
                      s"`${f.text}` takes ${plural(k, "type argument")}, but has 0", related = Some(d))
                  case None =>
                    report(f.text, s"${args.size} vs ${counts.mkString(" or ")} arguments", span, DiagnosticCode.ArityMismatch,
                      s"`${f.text}` is declared with ${counts.mkString(" or ")} ${if (counts == Vector(1)) "argument" else "arguments"}, but has ${args.size}",
                      related = Some(decls.head))
                }
                Ty.Unknown
              case Some(d) => position(f, declared(f, d, args, scope), asFormula, Some(d))
            }
      }
    }

    /** A defined word with arguments: `$ite`, `$distinct` and arithmetic. */
    private def defined(f: Name, args: Seq[Term], scope: Scope): Ty = f.text match {
      case "$ite" if args.size == 3 =>
        val c = term(args(0), scope)
        if (!Ty.compatible(c, Ty.Bool))
          report("$ite", s"condition: `${Ty.show(c)}`", args(0).span, DiagnosticCode.TypeMismatch,
            s"the condition of `$$ite` has type `${Ty.show(c)}`, but must be `$$o`")
        val (a, b) = (term(args(1), scope), term(args(2), scope))
        if (!Ty.compatible(a, b))
          report("$ite", s"`${Ty.show(a)}` vs `${Ty.show(b)}`", args(2).span, DiagnosticCode.TypeMismatch,
            s"the branches of `$$ite` have types `${Ty.show(a)}` and `${Ty.show(b)}`")
        if (a == Ty.Unknown) b else a
      case "$ite" =>
        args.foreach(term(_, scope))
        report("$ite", s"${args.size} vs 3 arguments", f.span, DiagnosticCode.ArityMismatch, s"`$$ite` takes 3 arguments, but has ${args.size}")
        Ty.Unknown
      case "$distinct" =>
        val tys = args.toVector.map(term(_, scope))
        tys.zipWithIndex.drop(1).foreach { case (t, i) =>
          if (!Ty.compatible(tys.head, t))
            report("$distinct", s"argument ${i + 1}: `${Ty.show(t)}` vs `${Ty.show(tys.head)}`", args(i).span, DiagnosticCode.TypeMismatch,
              s"argument ${i + 1} of `$$distinct` has type `${Ty.show(t)}`, but `$$distinct` expects `${Ty.show(tys.head)}` like argument 1")
        }
        Ty.Bool
      case _ =>
        Tff.arithmetic.get(f.text) match {
          case Some(a) => arithmetic(f, a, args, scope)
          case None    => args.foreach(term(_, scope)); Ty.Unknown // unknown words are reported by the local checks
        }
    }

    /** Arguments against a declaration; returns the (instantiated) result type. */
    private def declared(f: Name, d: Decl, args: Seq[Term], scope: Scope): Ty = {
      val (instance, rest) = resolved(d.ty) match {
        case Ty.Forall(vs, body) =>
          val typeArgs = args.take(vs.size).toVector.zipWithIndex.map { case (a, i) =>
            asType(a, scope).getOrElse {
              report(f.text, s"argument ${i + 1} is not a type", a.span, DiagnosticCode.WrongTypeArguments,
                s"argument ${i + 1} of `${f.text}` must be a type", related = Some(d))
              Ty.Unknown
            }
          }
          (Ty.substitute(body, vs.zip(typeArgs).toMap), args.drop(vs.size))
        case other => (other, args)
      }
      val expected = instance match {
        case Ty.Fun(as, _) => as
        case _             => Vector.empty
      }
      val offset = args.size - rest.size
      rest.zip(expected).zipWithIndex.foreach { case ((a, e), i) =>
        val actual = term(a, scope)
        val n = i + 1 + offset
        if (!Ty.compatible(e, actual))
          report(f.text, s"argument $n: `${Ty.show(actual)}` vs `${Ty.show(e)}`", a.span, DiagnosticCode.TypeMismatch,
            s"argument $n of `${f.text}` has type `${Ty.show(actual)}`, but `${f.text}` expects `${Ty.show(e)}`", related = Some(d))
      }
      Ty.result(instance)
    }

    /** A declared type with the types not declared at this point `Unknown` (reported on the declaration). */
    private def resolved(t: Ty): Ty = t match {
      case Ty.Con(n, args)  => if (n.startsWith("$") || types(n).nonEmpty) Ty.Con(n, args.map(resolved)) else Ty.Unknown
      case Ty.Fun(as, r)    => Ty.Fun(as.map(resolved), resolved(r))
      case Ty.Forall(vs, b) => Ty.Forall(vs, resolved(b))
      case Ty.Tuple(es)     => Ty.Tuple(es.map(resolved))
      case other            => other
    }

    /** A TF1 type argument written as a term: a defined type, a declared type applied to types, or a type variable.
      * An undeclared plain name is reported as an undeclared type and is `Unknown`. */
    private def asType(t: Term, scope: Scope): Option[Ty] = t match {
      case a: AtomicTerm if a.kind == AtomKind.Defined =>
        Tff.definedTypes.get(a.f.text).filter(_ != "$tType").map(Ty.Con(_, Vector.empty))
      case a: AtomicTerm if a.kind == AtomKind.Plain =>
        val args = a.args.toVector.map(asType(_, scope))
        if (!args.forall(_.isDefined)) None
        else
          types(a.f.key) match {
            case Vector() if symbols(a.f.key).isEmpty => undeclaredType(a.f, a.span); Some(Ty.Unknown)
            case Vector()                             => None
            case ts if ts.exists(d => Ty.arity(d.ty).contains(args.size)) => Some(Ty.Con(a.f.key, args.flatten))
            case ts =>
              val counts = ts.flatMap(d => Ty.arity(d.ty)).distinct.sorted
              report(a.f.text, s"${args.size} vs ${counts.mkString(" or ")} type arguments", a.span, DiagnosticCode.WrongTypeArguments,
                s"type `${a.f.text}` takes ${counts.map(plural(_, "argument")).mkString(" or ")}, but has ${args.size}")
              Some(Ty.Unknown)
          }
      case Variable(n, _) => Option.when(scope.get(n.key).contains(Ty.Kind))(Ty.Var(n.key))
      case _: ErrorNode | _: Unsupported => Some(Ty.Unknown)
      case _ => None
    }

    /** An undeclared symbol: used before its declaration, or TPTP default types (arguments `$i`; `$o` as a formula,
      * `$i` or a `$o` value as a term), or (in an axiom file) a warning. */
    private def undeclared(f: Name, args: Seq[Term], scope: Scope, asFormula: Boolean): Ty =
      sig.later(f.key) match {
        case Some(d) =>
          report(f.text, "used before its declaration", f.span, DiagnosticCode.UndeclaredSymbol,
            s"`${f.text}` is used before its declaration", related = Some(d))
          args.foreach(term(_, scope))
          Ty.Unknown
        case None if axiomFile =>
          if (warnedUndeclared.add(f.key))
            report(f.text, "undeclared", f.span, DiagnosticCode.UndeclaredSymbol,
              s"`${f.text}` is not declared in this file or its includes", Severity.Warning)
          args.foreach(term(_, scope))
          Ty.Unknown
        case None =>
          args.zipWithIndex.foreach { case (a, i) =>
            val actual = term(a, scope)
            if (!Ty.compatible(Ty.Individual, actual))
              report(f.text, s"argument ${i + 1}: `${Ty.show(actual)}` vs `$$i`", a.span, DiagnosticCode.TypeMismatch,
                s"argument ${i + 1} of `${f.text}` has type `${Ty.show(actual)}`, but `${f.text}` is not declared, so it expects `$$i`")
          }
          // as a term, `$i` or a `$o` value: TXF uses undeclared predicates as Boolean values (SWX228_1)
          if (asFormula) Ty.Bool else Ty.Default
      }

    /** A result in a formula position must be `$o`; a `$o` term is a TXF Boolean value and fine. */
    private def position(f: Name, result: Ty, asFormula: Boolean, d: Option[Decl]): Ty = {
      if (asFormula && result != Ty.Unknown && result != Ty.Bool)
        report(f.text, "function vs predicate", f.span, DiagnosticCode.PredicateFunctionClash,
          s"`${f.text}` is declared as a function, but used as a predicate here", related = d)
      result
    }

    private def arithmetic(f: Name, a: Tff.Arithmetic, args: Seq[Term], scope: Scope): Ty = {
      import Tff.Arithmetic.*
      val tys = args.toVector.map(term(_, scope))
      val expected = a match {
        case Compare | Quotient => 2
        case Test | To(_)       => 1
        case Same(n)            => n
      }
      if (tys.size != expected) {
        report(f.text, s"${tys.size} vs $expected arguments", f.span, DiagnosticCode.ArityMismatch,
          s"`${f.text}` takes ${plural(expected, "argument")}, but has ${tys.size}")
        Ty.Unknown
      } else {
        val one = a match { case Test | To(_) => false; case _ => true }
        var first: Option[(Ty, Int)] = None
        tys.zipWithIndex.foreach { case (t, i) =>
          t match {
            case Ty.Con(n, Vector()) if Tff.numericTypes(n) =>
              first match {
                case Some((ft, fi)) if one && ft != t =>
                  report(f.text, s"argument ${i + 1}: `${Ty.show(t)}` vs `${Ty.show(ft)}`", args(i).span, DiagnosticCode.TypeMismatch,
                    s"argument ${i + 1} of `${f.text}` has type `${Ty.show(t)}`, but `${f.text}` expects `${Ty.show(ft)}` like argument ${fi + 1}")
                case None => first = Some((t, i))
                case _    => ()
              }
            case Ty.Unknown => ()
            case other =>
              report(f.text, s"argument ${i + 1}: `${Ty.show(other)}` vs a number", args(i).span, DiagnosticCode.TypeMismatch,
                s"argument ${i + 1} of `${f.text}` has type `${Ty.show(other)}`, but `${f.text}` expects `$$int`, `$$rat` or `$$real`")
          }
        }
        a match {
          case Compare | Test => Ty.Bool
          case To(target)     => Ty.Con(target, Vector.empty)
          case Quotient =>
            first.foreach { case (t, i) =>
              if (t == Ty.Con("$int", Vector.empty))
                report(f.text, s"argument ${i + 1}: `$$int` vs `$$rat` or `$$real`", args(i).span, DiagnosticCode.TypeMismatch,
                  s"argument ${i + 1} of `${f.text}` has type `$$int`, but `${f.text}` expects `$$rat` or `$$real`")
            }
            first.fold(Ty.Unknown)(_._1)
          case Same(_) => first.fold(Ty.Unknown)(_._1)
        }
      }
    }

    private def sides(op: String, l: Term, r: Term, scope: Scope): Unit = {
      val (lt, rt) = (term(l, scope), term(r, scope))
      if (!Ty.compatible(lt, rt))
        report(op, s"`${Ty.show(lt)}` vs `${Ty.show(rt)}`", r.span, DiagnosticCode.TypeMismatch,
          s"the sides of `$op` have types `${Ty.show(lt)}` and `${Ty.show(rt)}`")
    }

    /** A syntax error inside the argument list but in no argument (a dropped `,`), as in FOF. */
    private def droppedToken(span: Span, args: Seq[Term]): Boolean =
      args.nonEmpty && {
        val argList = Span(math.max(span.start, args.head.span.start - 1), span.end)
        errors.exists(e => within(e, argList) && !args.exists(a => within(e, a.span)))
      }

    private def within(e: Span, s: Span): Boolean = e.start >= s.start && e.end <= s.end

    private def report(subject: String, brief: String, at: Span, code: DiagnosticCode, message: String,
                       severity: Severity = Severity.Error, related: Option[Decl] = None): Unit =
      problems += TypeProblem(subject, brief, Diagnostic(at, severity, code, message,
        related.toVector.map(d => RelatedInfo(d.at, s"declaration of `${d.symbol.text}`: ${Ty.show(d.ty)}"))))
  }

  /** `$int` for integers, `$rat` for rationals (`1/2`), `$real` for reals (`2.5`, `1E3`). */
  private def numberType(text: String): String =
    if (text.contains('/')) "$rat" else if (text.exists(c => c == '.' || c == 'e' || c == 'E')) "$real" else "$int"

  private def plural(n: Int, word: String): String = if (n == 1) s"1 $word" else s"$n ${word}s"

  private def role(r: TffRecord): Option[Diagnostic] = r.role.word.valid.flatMap { w =>
    val declaration = r.statement.isInstanceOf[TypeDeclaration]
    val missing = r.statement match { case Logical(_: ErrorNode, _) => true; case _ => false }
    val message =
      if (w.text == "type") Option.when(!declaration && !missing)("role `type` is only for type declarations")
      else if (declaration) Some("a type declaration needs role `type`")
      else if (Tff.roles(w.text)) None
      else if (w.text == "unknown") Some("role `unknown` marks an error situation")
      else if (Fof.bnfRoles(w.text)) Some(s"role `${w.text}` is not used in TFF")
      else Some(s"`${w.text}` is not a formula role")
    message.map(Diagnostic(w.span, Severity.Error, DiagnosticCode.InvalidRole, _))
  }

  private final class Binding(val name: Name) { var used = false }

  /** Variables and `$`-words, as `FofChecker.Walk` (free and unused variables, broken variable lists, error spans).
    * A type variable is used when it occurs in a type or as an argument; `$let` parameters are bound in their
    * definition's right-hand side and never reported as unused. */
  private final class LocalWalk(errors: Vector[Span], checkWords: Boolean) {
    val diagnostics = Vector.newBuilder[Diagnostic]

    def formula(f: Formula, scope: Map[String, Binding]): Unit = f match {
      case a: AtomicFormula =>
        if (a.kind == AtomKind.Defined) definedWord(a.f, if (a.args.isEmpty) Tff.definedPropositions else Tff.definedPredicates)
        a.args.foreach(term(_, scope))
      case Equality(l, r, _, _)   => term(l, scope); term(r, scope)
      case Inequality(l, r, _, _) => term(l, scope); term(r, scope)
      case q @ QuantifiedFormula(_, vars, body, _, _) =>
        // a binding's type may use the variables bound before it in the same list (`[T: $tType, X: T]`)
        var inner = scope
        val bindings = vars.toVector.flatMap { v =>
          v.ty.foreach(typeUses(_, inner, declaration = false))
          val b = v.name.valid.map(n => new Binding(n))
          b.foreach(b => inner += b.name.key -> b)
          b
        }
        val varList = Span(q.span.start, math.max(q.span.start, body.outer.start))
        if (errors.exists(e => within(e, varList))) inner += Opaque -> new Binding(Name(Opaque, q.span))
        formula(body, inner)
        val effective = bindings.groupBy(_.name.key).values.map(_.last).toSet
        if (!containsGap(body) && !errors.exists(e => within(e, body.outer)))
          bindings.filter(b => !b.used || !effective(b)).foreach { b =>
            diagnostics += Diagnostic(b.name.span, Severity.Warning, DiagnosticCode.UnusedVariable, s"variable `${b.name.text}` is never used")
          }
      case UnaryFormula(_, body, _, _)   => formula(body, scope)
      case BinaryFormula(_, l, r, _, _)  => formula(l, scope); formula(r, scope)
      case VariableFormula(n, _, _)      => use(n, scope, "variable", "a quantifier")
      case l: Let                        => let(l, scope)
      case _: ErrorNode | _: Unsupported => ()
    }

    def term(t: Term, scope: Map[String, Binding]): Unit = t match {
      case a: AtomicTerm =>
        if (a.kind == AtomKind.Defined) definedWord(a.f, termWords)
        a.args.foreach(term(_, scope))
      case Variable(name, _) => use(name, scope, "variable", "a quantifier")
      case FormulaTerm(f, _) => formula(f, scope)
      case Tuple(es, _)      => es.foreach(term(_, scope))
      case l: Let            => let(l, scope)
      case _: DistinctObject | _: NumberTerm | _: ErrorNode | _: Unsupported => ()
    }

    private def let(l: Let, scope: Map[String, Binding]): Unit = {
      l.typings.foreach(t => typeUses(t.ty, scope, declaration = true))
      l.definitions.foreach { d =>
        val params = d.lhs match {
          case a: AtomicTerm => a.args.collect { case Variable(n, _) => n.key -> new Binding(n) }.toMap
          case _             => Map.empty[String, Binding]
        }
        d.lhs match {
          case a: AtomicTerm => a.args.filterNot(_.isInstanceOf[Variable]).foreach(term(_, scope))
          case other         => term(other, scope)
        }
        term(d.rhs, scope ++ params)
      }
      term(l.body, scope)
    }

    /** A declaration's type: its type variables must be bound by `!>`. */
    def declaredType(t: Type): Unit = typeUses(t, Map.empty, declaration = true)

    private def typeUses(t: Type, scope: Map[String, Binding], declaration: Boolean): Unit = t match {
      case TypeApp(_, args, _)       => args.foreach(typeUses(_, scope, declaration))
      case TypeVariable(n, _)        => if (declaration) use(n, scope, "type variable", "`!>`") else use(n, scope, "variable", "a quantifier")
      case MappingType(args, res, _) => (args :+ res).foreach(typeUses(_, scope, declaration))
      case TupleType(es, _)          => es.foreach(typeUses(_, scope, declaration))
      case QuantifiedType(vars, body, _) =>
        typeUses(body, scope ++ vars.flatMap(_.name.valid).map(n => n.key -> new Binding(n)), declaration)
      case _: ErrorNode | _: Unsupported => ()
    }

    private def use(name: Name, scope: Map[String, Binding], what: String, binder: String): Unit =
      scope.get(name.key) match {
        case Some(b) => b.used = true
        case None if scope.contains(Opaque) => ()
        case None =>
          diagnostics += Diagnostic(name.span, Severity.Error, DiagnosticCode.FreeVariable, s"$what `${name.text}` is not bound by $binder")
      }

    private def definedWord(f: Name, allowed: Set[String]): Unit =
      if (checkWords && !allowed(f.text)) {
        val message = if (Fof.bnfDefinedWords(f.text)) s"`${f.text}` is not defined in TFF" else s"`${f.text}` is not a defined word"
        diagnostics += Diagnostic(f.span, Severity.Warning, DiagnosticCode.UnknownDefinedWord, message)
      }

    private def within(e: Span, s: Span): Boolean = e.start >= s.start && e.end <= s.end

    private def containsGap(t: Tree): Boolean = t match {
      case _: ErrorNode | _: Unsupported => true
      case other                         => other.children.exists(containsGap)
    }
  }
}
