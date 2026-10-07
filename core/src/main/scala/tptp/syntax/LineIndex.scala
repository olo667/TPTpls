package tptp.syntax

import scala.collection.mutable.ArrayBuffer

/** Zero-based line and UTF-16 code-unit column, as used by LSP. */
final case class Position(line: Int, character: Int)

/** Converts between code-point offsets and LSP positions. Line breaks are LF, CRLF and lone CR. */
final class LineIndex private (codePoints: Array[Int], lineStarts: Array[Int], lineEnds: Array[Int]) {
  def lineCount: Int = lineStarts.length
  def length: Int = codePoints.length

  def position(offset: Int): Position = {
    val o = offset.max(0).min(codePoints.length)
    val line = lineOf(o)
    var units = 0
    var i = lineStarts(line)
    while (i < o) {
      units += Character.charCount(codePoints(i))
      i += 1
    }
    Position(line, units)
  }

  def offset(pos: Position): Int =
    if (pos.line < 0) 0
    else if (pos.line >= lineStarts.length) codePoints.length
    else {
      val end = lineEnds(pos.line)
      var i = lineStarts(pos.line)
      var units = 0
      while (i < end && units + Character.charCount(codePoints(i)) <= pos.character) {
        units += Character.charCount(codePoints(i))
        i += 1
      }
      i
    }

  private def lineOf(offset: Int): Int = {
    val found = java.util.Arrays.binarySearch(lineStarts, offset)
    if (found >= 0) found else -found - 2
  }
}

object LineIndex {
  def apply(text: String): LineIndex = {
    val cps = text.codePoints().toArray
    val starts = ArrayBuffer(0)
    val ends = ArrayBuffer.empty[Int]
    var i = 0
    while (i < cps.length) {
      val c = cps(i)
      if (c == '\n') {
        ends += i
        starts += i + 1
      } else if (c == '\r') {
        ends += i
        if (i + 1 < cps.length && cps(i + 1) == '\n') i += 1
        starts += i + 1
      }
      i += 1
    }
    ends += cps.length
    new LineIndex(cps, starts.toArray, ends.toArray)
  }
}
