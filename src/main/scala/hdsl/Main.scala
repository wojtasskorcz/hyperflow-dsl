package hdsl

import hdsl.parser.HdslParser

object Main {
  
  def main(args: Array[String]) {
    println(util.Properties.versionString)
    println(HdslParser.parseAll(HdslParser.typedArg, "xpath: String"))
    println(HdslParser.parseAll(HdslParser.signal, "signal Config (xpath: String, start_time: String, end_time: String, baseTemp: String)"))
  }

}