import parser.Parser

object Main {
  
  def main(args: Array[String]) {
    println(util.Properties.versionString)
    println(Parser.parseAll(Parser.signal, "signal Config (xpath: String, start_time: String, end_time: String, baseTemp: String)"))
  }

}