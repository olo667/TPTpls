package tptp.server.workspace

import java.nio.file.Path
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*
import tptp.analysis.{DiskSources, SourceProvider, Stamp}

final case class OpenDocument(uri: String, path: Path, version: Int, text: String)

/** Open editor documents; thread-safe. As a SourceProvider, open documents shadow files on disk. */
final class DocumentStore extends SourceProvider {
  private val byUri = new ConcurrentHashMap[String, OpenDocument]()

  def open(uri: String, version: Int, text: String): Option[OpenDocument] =
    Uris.toPath(uri).map { p =>
      val d = OpenDocument(uri, p, version, text)
      byUri.put(uri, d)
      d
    }

  def change(uri: String, version: Int, text: String): Option[OpenDocument] =
    Option(byUri.computeIfPresent(uri, (_, d) => d.copy(version = version, text = text)))

  def close(uri: String): Option[OpenDocument] = Option(byUri.remove(uri))
  def get(uri: String): Option[OpenDocument] = Option(byUri.get(uri))
  def all: Vector[OpenDocument] = byUri.values.asScala.toVector

  private def byPath(p: Path): Option[OpenDocument] = byUri.values.asScala.find(_.path == p)

  def stamp(path: Path): Option[Stamp] =
    byPath(path).map(d => Stamp.Version(d.version)).orElse(DiskSources.stamp(path))

  def read(path: Path): Option[(String, Stamp)] =
    byPath(path).map(d => (d.text, Stamp.Version(d.version))).orElse(DiskSources.read(path))
}
