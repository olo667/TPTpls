package tptp.syntax

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Compares the printed CST and errors of every fixture with its `.cst` file. UPDATE_GOLDEN=1 rewrites them. */
class GoldenSuite extends munit.FunSuite {
  private val dir = Path.of("src/test/resources/syntax")
  private val update = sys.env.contains("UPDATE_GOLDEN")

  private def render(text: String): String = {
    val parsed = SyntaxParser.parse(text)
    val errors = parsed.errors.map(e => s"${CstPrinter.cause(e.cause)} ${e.span.start}..${e.span.end} ${e.message}")
    CstPrinter.print(parsed.cst) + "-- errors --\n" + errors.map(_ + "\n").mkString
  }

  private val fixtures: Vector[Path] =
    Using.resource(Files.list(dir))(_.iterator.asScala.filter(_.toString.endsWith(".p")).toVector.sortBy(_.toString))

  test("fixtures exist") { assert(fixtures.nonEmpty, s"no fixtures in ${dir.toAbsolutePath}") }

  fixtures.foreach { fixture =>
    test(s"golden: ${fixture.getFileName}") {
      val actual = render(Files.readString(fixture, UTF_8))
      val golden = Path.of(fixture.toString.stripSuffix(".p") + ".cst")
      if (update) Files.writeString(golden, actual, UTF_8)
      else {
        assert(Files.exists(golden), s"missing $golden; run with UPDATE_GOLDEN=1 and review the output")
        assertNoDiff(actual, Files.readString(golden, UTF_8))
      }
    }
  }

  test("a comment containing ** does not swallow records") {
    val parsed = SyntaxParser.parse(Files.readString(dir.resolve("star_comment.p"), UTF_8))
    assertEquals(parsed.errors, Vector.empty)
    assertEquals(parsed.cst.records.size, 2)
  }

  test("integers parse as names and terms") {
    val parsed = SyntaxParser.parse(Files.readString(dir.resolve("integers.p"), UTF_8))
    assertEquals(parsed.errors, Vector.empty)
    val integerTokens = CstPrinter.print(parsed.cst).linesIterator.count(_.trim.startsWith("Integer "))
    assertEquals(integerTokens, 3) // the name 1, and the terms 42 and -1
  }
}
