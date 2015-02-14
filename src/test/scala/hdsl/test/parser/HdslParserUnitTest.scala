package hdsl.test.parser

import java.io.InputStreamReader

import hdsl.parser.HdslParser
import hdsl.test.UnitSpec

class HdslParserUnitTest extends UnitSpec {

  test("Comet workflow should be accepted by the parser") {
    val parsingResult = HdslParser.parseAll(HdslParser.workflow,
      new InputStreamReader(getClass.getResourceAsStream("/comet.hdsl")))
    if (!parsingResult.successful) {
      println(parsingResult)
    }
    assert(parsingResult.successful)
  }

  test("Comet workflow with arrays should be accepted by the parser") {
    val parsingResult = HdslParser.parseAll(HdslParser.workflow,
      new InputStreamReader(getClass.getResourceAsStream("/comet_arrays.hdsl")))
    if (!parsingResult.successful) {
      println(parsingResult)
    }
    assert(parsingResult.successful)
  }

}
