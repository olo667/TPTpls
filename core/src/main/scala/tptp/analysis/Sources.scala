package tptp.analysis

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.util.Try

/** Identifies a version of a file's content. */
enum Stamp {
  /** An open editor document. */
  case Version(n: Int)
  /** A file on disk. */
  case Modified(millis: Long)
}

trait SourceProvider {
  def stamp(path: Path): Option[Stamp]
  def read(path: Path): Option[(String, Stamp)]
}

object DiskSources extends SourceProvider {
  def stamp(path: Path): Option[Stamp] =
    if (Files.isRegularFile(path)) Try(Stamp.Modified(Files.getLastModifiedTime(path).toMillis)).toOption else None

  def read(path: Path): Option[(String, Stamp)] =
    for {
      s <- stamp(path)
      bytes <- Try(Files.readAllBytes(path)).toOption
    } yield (new String(bytes, UTF_8), s)
}
