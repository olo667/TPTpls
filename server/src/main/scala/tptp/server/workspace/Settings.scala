package tptp.server.workspace

import com.google.gson.JsonObject
import java.nio.file.Path
import scala.util.Try

final case class Settings(tptpRoot: Option[Path], debounceMs: Long)

object Settings {
  val DefaultDebounceMs = 200L

  def default(env: Map[String, String]): Settings =
    Settings(env.get("TPTP").filter(_.nonEmpty).flatMap(p => Try(Path.of(p)).toOption), DefaultDebounceMs)

  /** Overrides fields present and valid in `json` (a Gson object, possibly wrapped as {"tptp": {...}}). */
  def merge(base: Settings, json: Any): Settings = json match {
    case o: JsonObject if o.has("tptp") && o.get("tptp").isJsonObject => merge(base, o.getAsJsonObject("tptp"))
    case o: JsonObject =>
      val root = Option(o.get("tptpRoot"))
        .filter(_.isJsonPrimitive)
        .flatMap(e => Try(e.getAsString).toOption)
        .filter(_.nonEmpty)
        .flatMap(p => Try(Path.of(p)).toOption)
      val debounce = Option(o.get("debounceMs"))
        .filter(_.isJsonPrimitive)
        .flatMap(e => Try(e.getAsLong).toOption)
        .filter(_ >= 0)
      Settings(root.orElse(base.tptpRoot), debounce.getOrElse(base.debounceMs))
    case _ => base
  }
}
