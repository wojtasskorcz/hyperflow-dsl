package hdsl.test.compiler

import java.io.InputStreamReader

import hdsl.compiler.HdslCompiler
import hdsl.parser.HdslParser
import hdsl.test.UnitSpec
import org.json4s.JsonAST.{JField, JObject, JString}
import org.json4s.NoTypeHints
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.write
import org.junit.Assert._

class HdslCompilerUnitTest extends UnitSpec {

  implicit val formats = Serialization.formats(NoTypeHints)

  test("That Comet workflow is properly compiled") {
    val parsingResult = HdslParser.parseAll(HdslParser.workflow,
      new InputStreamReader(getClass.getResourceAsStream("/comet.hdsl")))
    assert(parsingResult.successful)
    val outMap = new HdslCompiler().compile(parsingResult.get)
    val json = parse(write(outMap))

    // signals test

    val xmlData = new JObject((for {
      JObject(signal) <- json \ "signals"
      JField("name", JString("config")) <- signal
    } yield signal)(0))

    assertEquals("//Collection[@label='station']", ((xmlData \ "data")(0) \ "xpath").values)
    assertEquals("1.196499599E9", ((xmlData \ "data")(0) \ "start_time").values)

    // processes test

    val p = new JObject((for {
      JObject(process) <- json \ "processes"
      JField("name", JString("p")) <- process
    } yield process)(0))

    assertEquals("genXmlCollection", (p \ "function").values)
    assertEquals(true, (p \ "ordering").values)
    assertEquals("constantArgs", (p \ "config" \ "constantArgs").values)
    assertEquals("", (p \ "config" \ "args").values)
    assertEquals(List("xml", "config"), (p \ "ins").values)
    assertEquals(List("stations"), (p \ "outs").values)

    val partitionData = new JObject((for {
      JObject(process) <- json \ "processes"
      JField("function", JString("partitionData")) <- process
    } yield process)(0))

    assertEquals(List("stations"), (partitionData \ "ins").values)
    assertEquals(List("dataParts"), (partitionData \ "outs").values)

    val computeStats = new JObject((for {
      JObject(process) <- json \ "processes"
      JField("function", JString("computeStats")) <- process
    } yield process)(0))

    assertEquals(List("dataParts", "config"), (computeStats \ "ins").values)
    assertEquals(List("stats"), (computeStats \ "outs").values)

    val plotGraphs = new JObject((for {
      JObject(process) <- json \ "processes"
      JField("function", JString("plotData")) <- process
    } yield process)(0))

    assertEquals(List("stats"), (plotGraphs \ "ins").values)
    assertEquals(List("graph"), (plotGraphs \ "outs").values)

    val collectPlots = new JObject((for {
      JObject(process) <- json \ "processes"
      JField("function", JString("collectGraphs")) <- process
    } yield process)(0))

    assertEquals(List("graph"), (collectPlots \ "ins").values)
    assertEquals(Nil, (collectPlots \ "outs").values)
  }

}
