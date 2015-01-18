package hdsl.test.compiler

import hdsl.compiler.HdslCompiler
import hdsl.parser.HdslParser
import hdsl.test.UnitSpec

class SignalCompilationUnitTest extends UnitSpec {

  test("That passing too many arguments to SignalClass causes exception") {
    val wf =
      """
        |signal Config(xpath: String, start_time: String, end_time: String)
        |config = Config("//Collection[@label='station']", "1.196499599E9", "1.197359999E9", "10")
      """.stripMargin
    val parsingResult = HdslParser.parseAll(HdslParser.workflow, wf)
    assert(parsingResult.successful)
    intercept[RuntimeException] {
      new HdslCompiler().compile(parsingResult.get)
    }
  }

  test("That passing too little arguments to SignalClass causes exception") {
    val wf =
      """
        |signal Config(xpath: String, start_time: String, end_time: String, baseTemp: String)
        |config = Config("//Collection[@label='station']", "1.196499599E9", "1.197359999E9")
      """.stripMargin
    val parsingResult = HdslParser.parseAll(HdslParser.workflow, wf)
    assert(parsingResult.successful)
    intercept[RuntimeException] {
      new HdslCompiler().compile(parsingResult.get)
    }
  }

  test("That passing incompatible arguments to SignalClass causes exception") {
    val wf =
      """
        |signal Config(xpath: String, start_time: String, end_time: String, baseTemp: String)
        |config = Config("//Collection[@label='station']", "1.196499599E9", "1.197359999E9", 10)
      """.stripMargin
    val parsingResult = HdslParser.parseAll(HdslParser.workflow, wf)
    assert(parsingResult.successful)
    intercept[RuntimeException] {
      new HdslCompiler().compile(parsingResult.get)
    }
  }

  test("That passing String arguments to SignalClass works properly") {
    val wf =
      """
        |signal Config(xpath: String, start_time: String, end_time: String, baseTemp: String)
        |config = Config("//Collection[@label='station']", "1.196499599E9", "1.197359999E9", "10")
      """.stripMargin
    val parsingResult = HdslParser.parseAll(HdslParser.workflow, wf)
    assert(parsingResult.successful)
    new HdslCompiler().compile(parsingResult.get)
  }

  test("That multiple declarations of the same SignalClass cause exception") {
    val wf =
      """
        |signal XmlData(path: String)
        |signal Config(xpath: String, start_time: String, end_time: String, baseTemp: String)
        |signal XmlData(path: String)
      """.stripMargin
    val parsingResult = HdslParser.parseAll(HdslParser.workflow, wf)
    assert(parsingResult.successful)
    intercept[RuntimeException] {
      new HdslCompiler().compile(parsingResult.get)
    }
  }

}
