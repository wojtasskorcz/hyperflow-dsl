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

  test("That the workflow is properly compiled") {
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

    val anonymousPartitionData = new JObject((for {
      JObject(process) <- json \ "processes"
      JField("function", JString("partitionData")) <- process
    } yield process)(0))

    assertEquals(List("stations"), (anonymousPartitionData \ "ins").values)
    assertEquals(List("dataParts"), (anonymousPartitionData \ "outs").values)

    val anonymousComputeStats = new JObject((for {
      JObject(process) <- json \ "processes"
      JField("function", JString("computeStats")) <- process
    } yield process)(0))

    assertEquals(List("dataParts", "config"), (anonymousComputeStats \ "ins").values)
  }

}
