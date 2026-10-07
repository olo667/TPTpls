package tptp.server.workspace

import java.nio.file.{Files, Path}
import tptp.analysis.Stamp

class DocumentStoreSuite extends munit.FunSuite {
  test("open documents shadow disk content; closing falls back to disk") {
    val dir = Files.createTempDirectory("store").toRealPath()
    val file = Files.writeString(dir.resolve("a.p"), "disk")
    val store = DocumentStore()
    val uri = file.toUri.toString
    assertEquals(store.read(file).map(_._1), Some("disk"))
    store.open(uri, 1, "editor")
    assertEquals(store.read(file), Some(("editor", Stamp.Version(1))))
    store.change(uri, 2, "edited")
    assertEquals(store.stamp(file), Some(Stamp.Version(2)))
    assertEquals(store.close(uri).map(_.version), Some(2))
    assertEquals(store.read(file).map(_._1), Some("disk"))
  }
  test("non-file URIs are not stored") {
    val store = DocumentStore()
    assertEquals(store.open("untitled:Untitled-1", 1, "x"), None)
    assertEquals(store.all, Vector.empty)
  }
  test("Uris round-trip file paths") {
    val dir = Files.createTempDirectory("uris").toRealPath()
    val p = dir.resolve("with space.p")
    assertEquals(Uris.toPath(Uris.fromPath(p)), Some(p))
    assertEquals(Uris.toPath("untitled:x"), None)
    assertEquals(Uris.toPath("not a uri"), None)
  }
}
