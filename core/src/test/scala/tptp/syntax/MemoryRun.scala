package tptp.syntax

import java.lang.management.{ManagementFactory, MemoryType}
import java.nio.charset.StandardCharsets.ISO_8859_1
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Heap retained by a parse result and peak heap while parsing, in bytes per byte of text.
  * Not a test: `sbt "core/Test/runMain tptp.syntax.MemoryRun FILE..."`, then report the numbers.
  * The peak is the sum of the heap pools' peaks, so an upper bound. */
object MemoryRun {
  def main(args: Array[String]): Unit =
    for (arg <- args) {
      val path = Path.of(arg)
      val text = Files.readString(path, ISO_8859_1)
      SyntaxParser.parse(text) // warm up: classes and the prediction cache; the result is dropped
      val before = settledHeap()
      resetPeaks()
      val result = SyntaxParser.parse(text)
      val peak = peakHeap() - before
      val retained = settledHeap() - before
      println(f"${path.getFileName}%-20s ${text.length / 1024}%9d KB  retained ${retained.toDouble / text.length}%8.1f B/B  peak ${peak.toDouble / text.length}%8.1f B/B")
      java.lang.ref.Reference.reachabilityFence(result)
    }

  private def heapPools = ManagementFactory.getMemoryPoolMXBeans.asScala.filter(_.getType == MemoryType.HEAP)

  private def settledHeap(): Long = {
    (1 to 3).foreach(_ => System.gc())
    val r = Runtime.getRuntime
    r.totalMemory - r.freeMemory
  }

  private def resetPeaks(): Unit = heapPools.foreach(_.resetPeakUsage())
  private def peakHeap(): Long = heapPools.map(_.getPeakUsage.getUsed).sum
}
