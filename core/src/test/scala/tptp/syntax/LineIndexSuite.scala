package tptp.syntax

class LineIndexSuite extends munit.FunSuite {
  test("LF line endings") {
    val idx = LineIndex("ab\ncd")
    assertEquals(idx.position(3), Position(1, 0))
    assertEquals(idx.position(5), Position(1, 2))
    assertEquals(idx.offset(Position(1, 1)), 4)
    assertEquals(idx.lineCount, 2)
  }
  test("CRLF and lone CR count as one line break") {
    val crlf = LineIndex("a\r\nb")
    assertEquals(crlf.position(3), Position(1, 0))
    assertEquals(crlf.offset(Position(1, 0)), 3)
    val cr = LineIndex("a\rb")
    assertEquals(cr.position(2), Position(1, 0))
  }
  test("astral characters take two UTF-16 units but one offset") {
    val idx = LineIndex("x😀y")
    assertEquals(idx.length, 3)
    assertEquals(idx.position(2), Position(0, 3))
    assertEquals(idx.offset(Position(0, 3)), 2)
    assertEquals(idx.offset(Position(0, 2)), 1) // inside the surrogate pair: clamp to the character start
  }
  test("out-of-range input is clamped") {
    val idx = LineIndex("ab\ncd")
    assertEquals(idx.offset(Position(0, 99)), 2) // end of line content, before the terminator
    assertEquals(idx.offset(Position(9, 0)), 5)
    assertEquals(idx.offset(Position(-1, 0)), 0)
    assertEquals(idx.position(-4), Position(0, 0))
    assertEquals(idx.position(99), Position(1, 2))
  }
  test("empty text has one line") {
    val idx = LineIndex("")
    assertEquals(idx.lineCount, 1)
    assertEquals(idx.position(0), Position(0, 0))
  }
}
