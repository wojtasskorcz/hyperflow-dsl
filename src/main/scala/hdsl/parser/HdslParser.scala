package hdsl.parser

import hdsl.parser.structures.Signal

import scala.util.parsing.combinator.JavaTokenParsers

object HdslParser extends JavaTokenParsers {
  
  def workflow: Parser[Any] = rep(workflowElem)

  def workflowElem: Parser[Any] = signal | process | comment
  
  def signal: Parser[Signal] = "signal" ~> ident ~ ("(" ~> args <~ ")") ^^ {case name ~ args => new Signal(name, args)}

  def args: Parser[Map[String, String]] = repsep(arg, ",") ^^ (Map() ++ _)

  def arg: Parser[(String, String)] = ident ~ ":" ~ ident ^^ {case name ~ ":" ~ argType => (name, argType)}

  def process: Parser[Any] = "process" ~ ident ~ "(" ~ processArgs ~ ")" ~ opt(":" ~ ident) ~ "{" ~ "[^}]*".r ~ "}"

  def processArgs: Parser[Any] = repsep(processArg, ",")

  def processArg: Parser[Any] = argWithModifier | arg | argWithImplicitType

  def argWithModifier: Parser[Any] = ident ~ ":" ~ ident ~ ident

  def argWithImplicitType: Parser[Any] = ident ^^ {case name => (name, "Signal")}
  
  def comment: Parser[String] = "//.*".r

}