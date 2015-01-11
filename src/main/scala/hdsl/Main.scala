package hdsl

import hdsl.parser.HdslParser

object Main {
  
  def main(args: Array[String]) {
    println(util.Properties.versionString)
    println(HdslParser.parseAll(HdslParser.signalClassArg, "xpath: String"))
    println(HdslParser.parseAll(HdslParser.signalClass, "signal Config (xpath: String, start_time: String, end_time: String, baseTemp: String)"))
  }

}