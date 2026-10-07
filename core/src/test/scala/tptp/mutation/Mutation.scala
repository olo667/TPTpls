package tptp.mutation

import scala.util.Random
import tptp.syntax.*

/** @param causeIsGeneric the expectation accepts any cause, so "right cause, wrong place" is not meaningful */
enum MutationKind(val label: String, val mustError: Boolean, val causeIsGeneric: Boolean) {
  case DeleteElement extends MutationKind("delete element", true, false)
  case DeletePunctuation extends MutationKind("delete punctuation", true, false)
  case InsertStray extends MutationKind("insert stray token", true, false)
  case DuplicateComma extends MutationKind("duplicate comma", true, true)
  case Truncate extends MutationKind("truncate in record", true, true)
  case Typo extends MutationKind("typo", false, true)
}

/** A precise result: an error starting in `from..to` (code points of the mutated text) whose cause matches. */
final case class Expectation(from: Int, to: Int, cause: ErrorCause => Boolean, describe: String)

/** A mutated document and what parsing it should produce.
  * @param recordIndex    the original record that was mutated
  * @param boundaryRecord the first original record that may contain errors (one earlier for keyword typos)
  * @param recordsAfter   how many original records after the mutated one must parse exactly as before */
final case class Mutant(kind: MutationKind, description: String, text: String, recordIndex: Int, boundaryRecord: Int,
                        recordsAfter: Int, expected: Expectation)

/** Generates mutants of a valid document, deterministically for a seed. */
final class Mutator(seed: Long) {
  private val operatorChars = "!?&|~=<>+*-:@^"

  def mutants(text: String, parsed: ParsedFile, kind: MutationKind, count: Int): Vector[Mutant] = {
    val doc = Doc(text, parsed)
    val rnd = new Random(seed * 1000003L + kind.ordinal * 7919L + text.hashCode)
    val all = kind match {
      case MutationKind.DeleteElement     => deleteElements(doc)
      case MutationKind.DeletePunctuation => deletePunctuation(doc)
      case MutationKind.InsertStray       => insertStrays(doc)
      case MutationKind.DuplicateComma    => duplicateCommas(doc)
      case MutationKind.Truncate          => truncations(doc)
      case MutationKind.Typo              => typos(doc, rnd)
    }
    rnd.shuffle(all).take(count).map(_())
  }

  private def anyCause: ErrorCause => Boolean = _ => true

  private def deleteElements(doc: Doc): Vector[() => Mutant] = {
    val wanted = Set("term", "formula", "variable")
    def collect(c: Cst, parentChildren: Int): Vector[Node] = c match {
      case n: Node if n.kind.name == "annotations" => Vector.empty
      case n: Node =>
        val here =
          if (parentChildren > 1 && n.span.length > 0 && wanted(Grammar.Categories.describe(n.kind))) Vector(n) else Vector.empty
        here ++ n.children.flatMap(collect(_, n.children.size))
      case _ => Vector.empty
    }
    doc.records.zipWithIndex.flatMap { case (record, r) =>
      collect(record, 0).map { n =>
        val category = Grammar.Categories.describe(n.kind)
        val removed = n.span.length
        val gapEnd = doc.nextTokenStart(n.span.end) - removed
        () => Mutant(MutationKind.DeleteElement, s"delete $category '${doc.slice(n.span)}' at ${n.span.start}",
          doc.edit(n.span, ""), r, r, doc.after(r),
          Expectation(doc.previousTokenEnd(n.span.start), gapEnd, { case ErrorCause.MissingElement(c) => c == category; case _ => false }, s"missing $category"))
      }
    }
  }

  private def deletePunctuation(doc: Doc): Vector[() => Mutant] =
    doc.tokens.filter(t => t.text == ")" || t.text == "]" || t.text == ").").map { t =>
      val r = doc.recordOf(t.span.start)
      () => Mutant(MutationKind.DeletePunctuation, s"delete '${t.text}' at ${t.span.start}", doc.edit(t.span, ""), r, r, doc.after(r),
        Expectation(doc.previousTokenEnd(t.span.start), doc.nextTokenStart(t.span.end) - t.span.length,
          { case ErrorCause.MissingToken(expected) => expected.contains(t.kind); case _ => false }, s"missing '${t.text}'"))
    }

  private def insertStrays(doc: Doc): Vector[() => Mutant] =
    doc.tokens.map { t =>
      val r = doc.recordOf(t.span.start)
      val at = t.span.end + 1
      () => Mutant(MutationKind.InsertStray, s"insert '§' after '${t.text}' at ${t.span.end}", doc.edit(Span.empty(t.span.end), " §"),
        r, r, doc.after(r), Expectation(at, at, { case ErrorCause.UnexpectedToken(_) => true; case _ => false }, "unexpected '§'"))
    }

  private def duplicateCommas(doc: Doc): Vector[() => Mutant] =
    doc.tokens.filter(_.text == ",").map { t =>
      val r = doc.recordOf(t.span.start)
      () => Mutant(MutationKind.DuplicateComma, s"duplicate ',' at ${t.span.start}", doc.edit(Span.empty(t.span.end), ","), r, r,
        doc.after(r), Expectation(t.span.end, doc.nextTokenStart(t.span.end) + 1, anyCause, "an error at the extra comma"))
    }

  private def truncations(doc: Doc): Vector[() => Mutant] =
    doc.records.zipWithIndex.flatMap { case (record, r) =>
      val inside = doc.tokens.filter(t => record.span.contains(t.span.start))
      inside.indices.drop(1).dropRight(1).map { i =>
        val cut = inside(i).span.start
        () => Mutant(MutationKind.Truncate, s"truncate before '${inside(i).text}' at $cut", doc.prefix(cut), r, r, 0,
          // the repair may also drop the dangling last token, so its start counts as the end too
          Expectation(inside(i - 1).span.start, cut, anyCause, "an error at the end of the file"))
      }
    }

  private def typos(doc: Doc, rnd: Random): Vector[() => Mutant] = {
    val keywords = Grammar.Tokens.recordKeywords
    doc.tokens.flatMap { t =>
      val positions = t.text.indices.filter(i => t.text(i).isLetter || operatorChars.contains(t.text(i)))
      val isOperator = t.text.nonEmpty && t.text.forall(operatorChars.contains(_))
      val eligible = keywords(t.kind) || t.kind == Grammar.Tokens.lowerWord || t.kind == Grammar.Tokens.upperWord || isOperator
      if (!eligible || positions.isEmpty) None
      else {
        val i = positions(rnd.nextInt(positions.size))
        val old = t.text(i)
        val pool: Vector[Char] =
          if (old.isLetter) (if (old.isUpper) 'A' to 'Z' else 'a' to 'z').filter(_ != old).toVector
          else operatorChars.filter(_ != old).toVector
        val replacement = pool(rnd.nextInt(pool.size))
        val typo = t.text.updated(i, replacement)
        val r = doc.recordOf(t.span.start)
        val boundary = if (keywords(t.kind)) math.max(r - 1, 0) else r
        Some(() => Mutant(MutationKind.Typo, s"typo '${t.text}' -> '$typo' at ${t.span.start}", doc.edit(t.span, typo), r, boundary,
          doc.after(r), Expectation(t.span.start, t.span.end, anyCause, "an error at the typo")))
      }
    }
  }
}

/** A parsed valid document with helpers for editing it by code-point offsets. */
private final case class Doc(text: String, parsed: ParsedFile) {
  val records: Vector[Cst] = parsed.cst.records
  val tokens: Vector[Token] = LexerDriver.lex(text).tokens.map(t => Token(TokenKind(t.getType), t.getText, Span(t.getStartIndex, t.getStopIndex + 1)))
  private val length = text.codePointCount(0, text.length)

  private def index(cp: Int): Int = text.offsetByCodePoints(0, cp)
  def slice(span: Span): String = text.substring(index(span.start), index(span.end))
  def edit(span: Span, replacement: String): String = text.substring(0, index(span.start)) + replacement + text.substring(index(span.end))
  def prefix(cp: Int): String = text.substring(0, index(cp))
  def previousTokenEnd(before: Int): Int = tokens.takeWhile(_.span.end <= before).lastOption.map(_.span.end).getOrElse(0)
  def nextTokenStart(from: Int): Int = tokens.find(_.span.start >= from).map(_.span.start).getOrElse(length)
  def recordOf(offset: Int): Int = math.max(records.lastIndexWhere(_.span.start <= offset), 0)
  def after(r: Int): Int = records.size - r - 1
}
