package hdsl.parser

import hdsl.parser.structures.Signal

import scala.util.parsing.combinator.JavaTokenParsers

object HdslParser extends JavaTokenParsers {
  
  def workflow: Parser[Any] = rep(workflowElem)

  def workflowElem: Parser[Any] = signal | process | assignment | composition | comment
  
  def signal: Parser[Signal] = "signal" ~> ident ~ ("(" ~> args <~ ")") ^^ {case name ~ args => new Signal(name, args)}

  def args: Parser[Map[String, String]] = repsep(arg, ",") ^^ (Map() ++ _)

  def arg: Parser[(String, String)] = ident ~ ":" ~ ident ^^ {case name ~ ":" ~ argType => (name, argType)}

  def process: Parser[Any] = "process" ~ ident ~ "(" ~ processArgs ~ ")" ~ opt(":" ~ ident) ~ "{" ~ "[^}]*".r ~ "}"

  def processArgs: Parser[Any] = repsep(processArg, ",")

  def processArg: Parser[Any] = argWithModifier | arg | argWithImplicitType

  def argWithModifier: Parser[Any] = ident ~ ":" ~ ident ~ ident

  def argWithImplicitType: Parser[Any] = ident ^^ {case name => (name, "Signal")}

  def assignment: Parser[Any] = assignee ~ "=" ~ expr

  def assignee: Parser[Any] = singleAssignee | tupledAssignee

  def singleAssignee: Parser[Any] = rep1sep(ident, ".")

  def tupledAssignee: Parser[Any] = "(" ~ rep1sep(singleAssignee, ",") ~ ")"

  def expr: Parser[Any] = signalInstantiation | processInstantiation | atomicValue

  def signalInstantiation: Parser[Any] = ident ~ "(" ~ repsep(stringLiteral, ",") ~ ")"

  def processInstantiation = ident

  def atomicValue = "true" | "false" | stringLiteral

  def composition = compositionElem ~ "->" ~ rep1sep(compositionElem, "->")

  def compositionElem = ident ~ ":" ~ singleAssignee | ident | "(" ~ rep1sep(ident, ",") ~ ")"
  
  def comment: Parser[String] = "//.*".r

}