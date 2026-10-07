package tptp.server.workspace

import java.net.URI
import java.nio.file.Path
import scala.util.Try
import tptp.semantics.PathUtil

object Uris {
  def toPath(uri: String): Option[Path] =
    Try(URI.create(uri)).toOption
      .filter(u => u.getScheme == "file")
      .flatMap(u => Try(PathUtil.canonical(Path.of(u))).toOption)

  def fromPath(p: Path): String = p.toUri.toString
}
