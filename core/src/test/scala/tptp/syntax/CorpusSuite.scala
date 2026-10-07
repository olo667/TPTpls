package tptp.syntax

import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import java.nio.file.{Files, Path}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Parses every FOF problem and axiom file of a local TPTP library. Skipped unless TPTP is set. */
class CorpusSuite extends munit.FunSuite {
  override val munitTimeout: Duration = 30.minutes
  private val knownFailuresFile = Path.of("src/test/resources/corpus-known-failures.txt")

  test("TPTP library FOF files parse without syntax errors") {
    val root = sys.env.get("TPTP").map(Path.of(_)).filter(Files.isDirectory(_))
    assume(root.isDefined, "TPTP is not set to a TPTP library directory; skipping the corpus test")
    val base = root.get
    val files = Vector("Problems", "Axioms").map(base.resolve).filter(Files.isDirectory(_)).flatMap { d =>
      Using.resource(Files.walk(d)) {
        _.iterator.asScala.filter(p => Files.isRegularFile(p) && p.getFileName.toString.contains("+")).toVector
      }
    }
    val failing = files
      .filter(p => SyntaxParser.parse(Files.readString(p, ISO_8859_1)).errors.nonEmpty)
      .map(p => base.relativize(p).toString)
      .toSet
    val lines = Files.readAllLines(knownFailuresFile, UTF_8).asScala.toVector
    val header = lines.takeWhile(_.startsWith("#"))
    val known = lines.map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#")).toSet
    if (sys.env.contains("UPDATE_GOLDEN"))
      Files.write(knownFailuresFile, (header ++ failing.toVector.sorted).asJava, UTF_8)
    else {
      val unexpected = (failing -- known).toVector.sorted
      val fixed = (known -- failing).toVector.sorted
      assert(unexpected.isEmpty, s"${unexpected.size} of ${files.size} files fail to parse, e.g.\n${unexpected.take(20).mkString("\n")}")
      assert(fixed.isEmpty, s"these now parse; remove them from $knownFailuresFile:\n${fixed.mkString("\n")}")
    }
  }
}
