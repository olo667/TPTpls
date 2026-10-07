package tptp.semantics

import java.nio.file.{Files, Path}
import scala.util.Try

final case class ResolutionEnv(workspaceRoots: Vector[Path], tptpRoot: Option[Path])

object PathUtil {
  /** Real path (symlinks resolved) if the file exists, otherwise the absolute normalized path. */
  def canonical(p: Path): Path = Try(p.toRealPath()).getOrElse(p.toAbsolutePath.normalize)
}

object IncludeResolver {
  def searchDirs(includingDir: Path, env: ResolutionEnv): Vector[Path] =
    (includingDir +: env.workspaceRoots) ++ env.tptpRoot.toVector

  /** The first regular file named `fileName` in the search order, as a canonical path. */
  def resolve(fileName: String, includingDir: Path, env: ResolutionEnv): Option[Path] =
    if (fileName.isEmpty) None
    else
      searchDirs(includingDir, env).iterator
        .flatMap(dir => Try(dir.resolve(fileName).normalize).toOption)
        .find(p => Files.isRegularFile(p))
        .map(PathUtil.canonical)
}
