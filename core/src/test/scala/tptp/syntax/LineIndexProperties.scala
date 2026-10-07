package tptp.syntax

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class LineIndexProperties extends munit.ScalaCheckSuite {
  private val piece = Gen.oneOf("a", "é", "😀", " ", "\n", "\r\n", "\r")
  private val text = Gen.listOf(piece).map(_.mkString)

  property("offset(position(o)) == o for every offset not inside a CRLF pair") {
    forAll(text) { s =>
      val cps = s.codePoints().toArray
      val idx = LineIndex(s)
      (0 to cps.length).forall { o =>
        val insideCrlf = o > 0 && o < cps.length && cps(o - 1) == '\r' && cps(o) == '\n'
        insideCrlf || idx.offset(idx.position(o)) == o
      }
    }
  }
}
