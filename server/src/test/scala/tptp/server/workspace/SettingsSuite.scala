package tptp.server.workspace

import com.google.gson.JsonParser
import java.nio.file.Path

class SettingsSuite extends munit.FunSuite {
  test("default reads TPTP from the environment") {
    assertEquals(Settings.default(Map("TPTP" -> "/opt/tptp")), Settings(Some(Path.of("/opt/tptp")), 200))
    assertEquals(Settings.default(Map.empty), Settings(None, 200))
  }
  test("merge overrides only present, valid fields") {
    val base = Settings(Some(Path.of("/a")), 200)
    assertEquals(Settings.merge(base, JsonParser.parseString("""{"debounceMs": 50}""")), Settings(Some(Path.of("/a")), 50))
    assertEquals(Settings.merge(base, JsonParser.parseString("""{"tptpRoot": "/b"}""")), Settings(Some(Path.of("/b")), 200))
    assertEquals(Settings.merge(base, JsonParser.parseString("""{"tptp": {"tptpRoot": "/c"}}""")), Settings(Some(Path.of("/c")), 200))
    assertEquals(Settings.merge(base, JsonParser.parseString("""{"debounceMs": -1, "tptpRoot": ""}""")), base)
    assertEquals(Settings.merge(base, null), base)
    assertEquals(Settings.merge(base, "nonsense"), base)
  }
}
