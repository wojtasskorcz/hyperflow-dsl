package hdsl.parser

import hdsl.parser.structures.{Process, Arg, Signal}

import scala.util.parsing.combinator.JavaTokenParsers

object HdslParser extends JavaTokenParsers {
  
  def workflow: Parser[Any] = rep(workflowElem)

  def workflowElem: Parser[Any] = signal | process | assignment | composition | comment
  
  def signal: Parser[Signal] = "signal" ~> ident ~ ("(" ~> typedArgs <~ ")") ^^ {case name ~ args => Signal(name, args)}

  def typedArgs: Parser[List[Arg]] = repsep(typedArg, ",") ^^ (List() ++ _)

  def typedArg: Parser[Arg] = ident ~ ":" ~ ident ^^ {case name ~ ":" ~ argType => Arg(name, argType, Nil)}

  def process: Parser[Process] = "process" ~> ident ~ ("(" ~> processArgs <~ ")") ~ opt(":" ~> ident) <~ "{" ~ "[^}]*".r ~ "}" ^^ {
    case name ~ args ~ None => Process(name, args, null)
    case name ~ args ~ Some(returnType) => Process(name, args, returnType)
  }

  def processArgs: Parser[List[Arg]] = repsep(processArg, ",")

  def processArg: Parser[Arg] = argWithModifiers | argWithImplicitType

  def argWithModifiers: Parser[Arg] = ident ~ ":" ~ rep(modifier) ~ ident ^^ {
    case name ~ ":" ~ modifiers ~ argType => Arg(name, argType, modifiers)
  }

  def modifier: Parser[String] = "sticky"

  def argWithImplicitType: Parser[Arg] = ident ^^ {case name => Arg(name, "Signal", Nil)}

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