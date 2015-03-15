package hdsl.test.compiler

import java.io.InputStreamReader

import hdsl.compiler.HdslCompiler
import hdsl.parser.HdslParser
import hdsl.test.UnitSpec
import org.json4s.JsonAST.{JField, JObject, JString}
import org.json4s.{JValue, NoTypeHints}
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
    val outMap = HdslCompiler.compile(parsingResult.get)
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
    ensureSignal("stations", json)
    ensureSignal(pCountSignalName, json)

    val partitionData = new JObject((for {
      JObject(process) <- json \ "processes"
      JField("function", JString("partitionData")) <- process
    } yield process)(0))

    assertEquals(List("stations"), (partitionData \ "ins").values)
    assertEquals(List("dataParts"), (partitionData \ "outs").values)
    ensureSignal("dataParts", json)

    val computeStats = new JObject((for {
      JObject(process) <- json \ "processes"
      JField("function", JString("computeStats")) <- process
    } yield process)(0))

    assertEquals(List("dataParts", "config"), (computeStats \ "ins").values)
    assertEquals(List("config"), (computeStats \ "sticky").values)
    assertEquals(List("stats"), (computeStats \ "outs").values)
    ensureSignal("stats", json)

    val plotGraphs = new JObject((for {
      JObject(process) <- json \ "processes"
      JField("function", JString("plotData")) <- process
    } yield process)(0))

    assertEquals(List("stats"), (plotGraphs \ "ins").values)
    assertEquals(List("graph"), (plotGraphs \ "outs").values)
    ensureSignal("graph", json)

    val collectPlots = new JObject((for {
      JObject(process) <- json \ "processes"
      JField("function", JString("collectGraphs")) <- process
    } yield process)(0))

    assertEquals(List("graph:" + pCountSignalName), (collectPlots \ "ins").values)
    assertEquals(Nil, (collectPlots \ "outs").values)
  }

  test("That comet_arrays workflow is properly compiled") {
    val n = 3
    val parsingResult = HdslParser.parseAll(HdslParser.workflow,
      new InputStreamReader(getClass.getResourceAsStream("/comet_arrays.hdsl")))
    assert(parsingResult.successful)
    val outMap = HdslCompiler.compile(parsingResult.get)
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

    val ps: List[JObject] = (for {
      JObject(process) <- json \ "processes"
      JField("function", JString("genXmlCollection")) <- process
    } yield process).map(new JObject(_))

    var psOuts = List[String]()
    var psCounts = List[String]()
    ps.foreach(p => {
//      println(pretty(render(p)))
      assertEquals(true, (p \ "ordering").values)
      assertEquals("", (p \ "config" \ "args").values)
      assertEquals(List("xml", "config"), (p \ "ins").values)
      val pOuts = (p \ "outs").values.asInstanceOf[List[String]]
      assertEquals(1, pOuts.length)
      val pOutSignalParts = pOuts(0).split(':')
      val (pOutSignal, pCountSignalName) = (pOutSignalParts(0), pOutSignalParts(1))
      psOuts :+= pOutSignal
      psCounts :+= pCountSignalName
      ensureSignal(pOutSignal, json)
      ensureSignal(pCountSignalName, json)
    })

    assertEquals(n, ps.size)
    assertEquals(n, ps.map(p => (p \ "name").values).distinct.size)
    assertEquals(n, psOuts.distinct.size)

    val partitionDatas: List[JObject] = (for {
      JObject(process) <- json \ "processes"
      JField("function", JString("partitionData")) <- process
    } yield process).map(new JObject(_))

    var partitionDatasOuts = List[String]()
    for ((p, idx) <- partitionDatas.view.zipWithIndex.force) {
      assertEquals(List(psOuts(idx)), (p \ "ins").values)
      val pOuts = (p \ "outs").values.asInstanceOf[List[String]]
      assertEquals(1, pOuts.length)
      ensureSignal(pOuts(0), json)
      partitionDatasOuts :+= pOuts(0)
    }

    assertEquals(n, partitionDatas.size)
    assertEquals(n, partitionDatas.map(p => (p \ "name").values).distinct.size)
    assertEquals(n, partitionDatas.map(p => (p \ "ins")(0).values).distinct.size)
    assertEquals(n, partitionDatasOuts.distinct.size)

    val computeStats: List[JObject] = (for {
      JObject(process) <- json \ "processes"
      JField("function", JString("computeStats")) <- process
    } yield process).map(new JObject(_))

    var computeStatsOuts = List[String]()
    for ((p, idx) <- computeStats.view.zipWithIndex.force) {
      assertEquals(List(partitionDatasOuts(idx), "config"), (p \ "ins").values)
      assertEquals(List("config"), (p \ "sticky").values)
      val pOuts = (p \ "outs").values.asInstanceOf[List[String]]
      assertEquals(1, pOuts.length)
      ensureSignal(pOuts(0), json)
      computeStatsOuts :+= pOuts(0)
    }

    assertEquals(n, computeStats.size)
    assertEquals(n, computeStats.map(p => (p \ "name").values).distinct.size)
    assertEquals(n, computeStats.map(p => (p \ "ins")(0).values).distinct.size)
    assertEquals(n, computeStatsOuts.distinct.size)

    val plotGraphs: List[JObject] = (for {
      JObject(process) <- json \ "processes"
      JField("function", JString("plotData")) <- process
    } yield process).map(new JObject(_))

    var plotGraphsOuts = List[String]()
    for ((p, idx) <- plotGraphs.view.zipWithIndex.force) {
      assertEquals(List(computeStatsOuts(idx)), (p \ "ins").values)
      val pOuts = (p \ "outs").values.asInstanceOf[List[String]]
      assertEquals(1, pOuts.length)
      ensureSignal(pOuts(0), json)
      plotGraphsOuts :+= pOuts(0)
    }

    assertEquals(n, plotGraphs.size)
    assertEquals(n, plotGraphs.map(p => (p \ "name").values).distinct.size)
    assertEquals(n, plotGraphs.map(p => (p \ "ins")(0).values).distinct.size)
    assertEquals(n, plotGraphsOuts.distinct.size)

    val collectPlots: List[JObject] = (for {
      JObject(process) <- json \ "processes"
      JField("function", JString("collectGraphs")) <- process
    } yield process).map(new JObject(_))

    for ((p, idx) <- collectPlots.view.zipWithIndex.force) {
      assertEquals(List(s"${plotGraphsOuts(idx)}:${psCounts(idx)}"), (p \ "ins").values)
      assertEquals(Nil, (p \ "outs").values)
    }

    assertEquals(n, collectPlots.size)
    assertEquals(n, collectPlots.map(p => (p \ "name").values).distinct.size)
    assertEquals(n, collectPlots.map(p => (p \ "ins")(0).values).distinct.size)
  }

  test("That branch_merge workflow is properly compiled") {
    val parsingResult = HdslParser.parseAll(HdslParser.workflow,
      new InputStreamReader(getClass.getResourceAsStream("/branch_merge.hdsl")))
    assert(parsingResult.successful)
    val outMap = HdslCompiler.compile(parsingResult.get)
    val json = parse(write(outMap))

    val generateBranches = new JObject((for {
      JObject(process) <- json \ "processes"
      JField("function", JString("generateBranches")) <- process
    } yield process)(0))

    val generateBranchesOuts = (generateBranches \ "outs").values.asInstanceOf[List[String]]
    assertEquals(4, generateBranchesOuts.size)
    assertEquals(List("branch1", "branch2", "branch3"), generateBranchesOuts.take(3))
    List("branch1", "branch2", "branch3").foreach(signal => ensureSignal(signal, json))
    val mergeSignalName = generateBranchesOuts(3)
    ensureSignal(mergeSignalName, json)
    assertEquals("choice", (generateBranches \ "type").values)

    val echos: List[JObject] = (for {
      JObject(process) <- json \ "processes"
      JField("function", JString("echo")) <- process
    } yield process).map(new JObject(_))

    for ((p, idx) <- echos.view.zipWithIndex.force) {
      assertEquals(List(s"branch${idx+1}"), (p \ "ins").values)
      assertEquals(List(s"outBranch${idx+1}"), (p \ "outs").values)
      assertEquals("dataflow", (p \ "type").values)
      ensureSignal(s"outBranch${idx+1}", json)
    }
    assertEquals(3, echos.map(p => (p \ "name").values).distinct.size)

    val gatherBranches = new JObject((for {
      JObject(process) <- json \ "processes"
      JField("function", JString("gatherBranches")) <- process
    } yield process)(0))

    val gatherBranchesIns = (gatherBranches \ "ins").values.asInstanceOf[List[String]]
    assertEquals(4, gatherBranchesIns.size)
    assertEquals(List("outBranch1", "outBranch2", "outBranch3"), gatherBranchesIns.take(3))
    assertEquals(mergeSignalName, gatherBranchesIns(3))
    assertEquals("join", (gatherBranches \ "type").values)
  }

  private def ensureSignal(name: String, json: JValue) = {
    (for {
      JObject(signal) <- json \ "signals"
      JField("name", JString(`name`)) <- signal
    } yield signal)(0)
  }

}
