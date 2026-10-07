package tptp.syntax

class PredictionCacheSuite extends munit.FunSuite {
  private val broken = (1 to 300).map(i => s"fof(a$i, axiom, ${"(" * (i % 7)}p$i & ${"q(X," * (i % 5)} )).\n").mkString

  test("the prediction cache is reset once it exceeds its limit") {
    PredictionCache.reset()
    val resetsBefore = PredictionCache.resets
    val limit = 50
    (1 to 5).foreach(_ => SyntaxParser.parse(broken, PredictionCache.Limit(limit)))
    assert(PredictionCache.states <= limit, s"states=${PredictionCache.states}")
    assert(PredictionCache.resets > resetsBefore, "the cache should have been reset while parsing")
  }

  test("parse results do not depend on the cache") {
    PredictionCache.reset()
    val cold = SyntaxParser.parse(broken)
    val warm = SyntaxParser.parse(broken)
    PredictionCache.reset()
    val again = SyntaxParser.parse(broken)
    def result(p: ParsedFile) = (p.cst.records, p.errors) // LineIndex has no structural equality
    assertEquals(result(warm), result(cold))
    assertEquals(result(again), result(cold))
  }
}
