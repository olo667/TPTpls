package tptp.syntax.tree

import java.nio.charset.StandardCharsets.ISO_8859_1
import java.nio.file.{Files, Path}
import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror
import scala.jdk.CollectionConverters.*
import scala.util.Using
import tptp.syntax.SyntaxParser

/** `children` is hand-written per class; a forgotten field would silently drop nodes from navigation and every walk.
  * Checks it against the fields on real trees, and checks that the trees keep no ANTLR objects alive. */
class TreeWalkSuite extends munit.FunSuite {
  private val extraInputs = Vector(
    "fof(s, axiom, [p, q] --> [r]).",
    "fof(d, axiom-x, p(f(\"o\", 42)) & a != b & ~ c = d & ! [X] : ? [Y] : q(X, Y)).",
    "fof(e, axiom, p(X,)).",
    "fof(f, axiom, (p & )).",
    "fof(, axiom, ! [X @ @ @ @ Y] : p).",
    "junk fof(g, axiom, p).",
    "thf(h, type, c: $i).",
    "include('x.ax', [a, b], sp).",
  )

  private def files: Vector[String] = {
    def under(dir: String) = Using.resource(Files.walk(Path.of(dir)))(_.iterator.asScala.filter(Files.isRegularFile(_)).toVector)
    (under("src/test/resources/syntax").filter(_.toString.endsWith(".p")) ++ under("src/test/resources/corpus"))
      .map(Files.readString(_, ISO_8859_1))
  }

  private lazy val trees: Vector[Tree] =
    (files ++ extraInputs).flatMap(t => SyntaxParser.parse(t).records).flatMap(all)

  private def all(t: Tree): Vector[Tree] = t +: t.children.toVector.flatMap(all)

  private def fieldTrees(v: Any): Vector[Tree] = v match {
    case t: Tree      => Vector(t)
    case o: Option[?] => o.toVector.flatMap(fieldTrees)
    case s: Seq[?]    => s.toVector.flatMap(fieldTrees)
    case _            => Vector.empty
  }

  private def holdsAntlr(v: Any): Boolean = v match {
    case null                => false
    case s: String           => false
    case p: Product          => p.productIterator.exists(holdsAntlr)
    case i: Iterable[?]      => i.exists(holdsAntlr)
    case x                   => x.getClass.getName.startsWith("org.antlr")
  }

  test("children are exactly the tree-valued fields, in field order") {
    trees.foreach { t =>
      val fields = t.asInstanceOf[Product].productIterator.toVector.flatMap(fieldTrees)
      assertEquals(t.children.toVector, fields, t.getClass.getSimpleName)
    }
  }

  test("no node holds an ANTLR object") {
    trees.foreach(t => assert(!holdsAntlr(t), t.getClass.getSimpleName))
  }

  test("every concrete node class occurs, so none is left unchecked") {
    val expected = TreeWalkSuite.concrete[Formula] ++ TreeWalkSuite.concrete[Term] ++ TreeWalkSuite.concrete[Statement] ++
      TreeWalkSuite.concrete[Record] ++ TreeWalkSuite.concrete[NameNode] ++ Vector("Op", "Role")
    val seen = trees.map(_.getClass.getSimpleName).toSet
    assertEquals(expected.toSet -- seen, Set.empty[String])
  }
}

object TreeWalkSuite {
  /** Names of the case classes below a sealed family, through its sealed sub-traits. */
  inline def concrete[T]: Vector[String] = inline summonInline[Mirror.Of[T]] match {
    case s: Mirror.SumOf[T]     => all[s.MirroredElemTypes].distinct
    case p: Mirror.ProductOf[T] => Vector(constValue[p.MirroredLabel])
  }

  private inline def all[Ts <: Tuple]: Vector[String] = inline erasedValue[Ts] match {
    case _: EmptyTuple => Vector.empty
    case _: (t *: ts)  => concrete[t] ++ all[ts]
  }
}
