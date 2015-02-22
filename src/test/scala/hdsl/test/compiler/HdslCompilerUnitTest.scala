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

    val config = new JObject((for {
      JObject(signal) <- json \ "signals"
      JField("name", JString("config")) <- signal
    } yield signal)(0))

    assertEquals("//Collection[@label='station']", ((config \ "data")(0) \ "xpath").values)
    assertEquals("1.196499599E9", ((config \ "data")(0) \ "start_time").values)

    val xml = new JObject((for {
      JObject(signal) <- json \ "signals"
      JField("name", JString("xml")) <- signal
    } yield signal)(0))

    assertEquals("data.xml", ((xml \ "data")(0) \ "path").values)

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
    val pOuts = (p \ "outs").values.asInstanceOf[List[String]]
    assertEquals(1, pOuts.length)
    val pOutSignalParts = pOuts(0).split(':')
    val (pOutSignal, pCountSignalName) = (pOutSignalParts(0), pOutSignalParts(1))
    assertEquals("stations", pOutSignal)
    // check if the count signal was declared in signals array
    val pCountSignal = new JObject((for {
      JObject(signal) <- json \ "signals"
      JField("name", JString(`pCountSignalName`)) <- signal
    } yield signal)(0))
    assertEquals(pCountSignalName, (pCountSignal \ "name").values)

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

    assertEquals(List("graph:" + pCountSignalName), (collectPlots \ "ins").values)
    assertEquals(Nil, (collectPlots \ "outs").values)
  }

  test("That comet_arrays workflow is properly compiled") {
    val parsingResult = HdslParser.parseAll(HdslParser.workflow,
      new InputStreamReader(getClass.getResourceAsStream("/comet_arrays.hdsl")))
    assert(parsingResult.successful)
    val outMap = new HdslCompiler().compile(parsingResult.get)
    val json = parse(write(outMap))

    // signals test

    val config = new JObject((for {
      JObject(signal) <- json \ "signals"
      JField("name", JString("config")) <- signal
    } yield signal)(0))

    assertEquals("//Collection[@label='station']", ((config \ "data")(0) \ "xpath").values)
    assertEquals("1.196499599E9", ((config \ "data")(0) \ "start_time").values)

    val xml = new JObject((for {
      JObject(signal) <- json \ "signals"
      JField("name", JString("xml")) <- signal
    } yield signal)(0))

    assertEquals("data.xml", ((xml \ "data")(0) \ "path").values)

    // processes test

    val ps: List[List[JField]] = for {
      JObject(process) <- json \ "processes"
      JField("function", JString("genXmlCollection")) <- process
    } yield process
    ps.foreach(rawP => {
      val p = new JObject(rawP)
      assertEquals("genXmlCollection", (p \ "function").values)
//      assertEquals(true, (p \ "ordering").values)
//      assertEquals("", (p \ "config" \ "args").values)
    })
    assertEquals(10, ps.size)
  }

}
