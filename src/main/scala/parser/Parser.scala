package parser

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator._

object Parser extends JavaTokenParsers {
  
  def signal: Parser[String] = "signal" ~> ident <~ "(" ~ repsep(arg, ",") ~ ")"
  
  def arg: Parser[Any] = ident ~ ":" ~ ident

}