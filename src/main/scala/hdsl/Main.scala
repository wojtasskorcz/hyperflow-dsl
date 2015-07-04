package hdsl

import java.io.{BufferedReader, FileReader}

import hdsl.compiler.HdslCompiler
import hdsl.parser.HdslParser
import org.json4s.NoTypeHints
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import org.json4s.native.Serialization._

object Main {

  implicit val formats = Serialization.formats(NoTypeHints)
  
  def main(args: Array[String]) = {
    if (args.length != 1) {
      println("Usage: sbt run <path/to/workflow.hdsl>")
      System.exit(1)
    }

    val parsingResult = HdslParser.parseAll(HdslParser.workflow,
      new BufferedReader(new FileReader(args(0))))
    if (!parsingResult.successful) {
      println("ERROR: Parsing failed:\n" + parsingResult)
    }
    val outMap = HdslCompiler.compile(parsingResult.get)
    val json = parse(write(outMap))
    println(pretty(render(json)))
  }

}