package parser

import scala.util.parsing.combinator._
import parser.structures._

object HdslParser extends JavaTokenParsers {
  
  def workflow: Parser[Any] = rep(workflowElem)
  
  def workflowElem: Parser[Any] = signal | comment
  
  def signal: Parser[Signal] = "signal" ~> ident ~ ("(" ~> args <~ ")") ^^ {case name ~ args => new Signal(name, args)}
  
  def args: Parser[Map[String, String]] = repsep(arg, ",") ^^ (Map() ++ _)
  
  def arg: Parser[Tuple2[String, String]] = ident ~ ":" ~ ident ^^ {case name ~ ":" ~ argType => (name, argType)}
  
  def comment: Parser[String] = "//.*".r

}