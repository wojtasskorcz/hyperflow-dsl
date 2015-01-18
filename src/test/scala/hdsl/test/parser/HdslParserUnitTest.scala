package hdsl.test.parser

import java.io.InputStreamReader
import hdsl.compiler.HdslCompiler
import hdsl.parser.HdslParser
import hdsl.test.UnitSpec

class HdslParserUnitTest extends UnitSpec {

  test("Comet workflow should be accepted by the parser") {
    val parsingResult = HdslParser.parseAll(HdslParser.workflow,
      new InputStreamReader(getClass.getResourceAsStream("/comet.hdsl")))
    assert(parsingResult.successful)
    new HdslCompiler().compile(parsingResult.get)
  }

}
