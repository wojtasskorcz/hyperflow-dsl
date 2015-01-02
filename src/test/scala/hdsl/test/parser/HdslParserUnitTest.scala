package hdsl.test.parser

import java.io.InputStreamReader

import hdsl.parser.HdslParser
import hdsl.test.UnitSpec

class HdslParserUnitTest extends UnitSpec {

  test("Comet workflow should be accepted by the parser") {
    assert(HdslParser.parseAll(HdslParser.workflow, new InputStreamReader(getClass.getResourceAsStream("/comet.hdsl"))).successful)
  }

}
