package hdsl.test.parser

import java.io.InputStreamReader

import hdsl.parser.HdslParser
import hdsl.test.UnitSpec

class HdslParserUnitTest extends UnitSpec {

  test("Comet workflow should be accepted by the parser") {
    testParsing("/comet.hdsl")
  }

  test("Comet workflow with arrays should be accepted by the parser") {
    testParsing("/comet_arrays.hdsl")
  }

  test("branch_merge workflow should be accepted by the parser") {
    testParsing("/branch_merge.hdsl")
  }

  private def testParsing(filename: String) = {
    val parsingResult = HdslParser.parseAll(HdslParser.workflow,
      new InputStreamReader(getClass.getResourceAsStream(filename)))
    if (!parsingResult.successful) {
      println(parsingResult)
    }
    assert(parsingResult.successful)
  }

}
