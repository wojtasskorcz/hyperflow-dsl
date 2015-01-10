package hdsl.parser

import java.io.Serializable

import hdsl.parser.structures._

import scala.util.parsing.combinator.JavaTokenParsers

object HdslParser extends JavaTokenParsers {
  
  def workflow: Parser[Any] = rep(workflowElem)

  def workflowElem: Parser[Any] = signal | process | assignment | composition | comment
  
  def signal: Parser[Signal] = "signal" ~> ident ~ ("(" ~> typedArgs <~ ")") ^^ {case name ~ args => Signal(name, args)}

  def typedArgs: Parser[List[Arg]] = repsep(typedArg, ",") ^^ (List() ++ _)

  def typedArg: Parser[Arg] = ident ~ ":" ~ ident ^^ {case name ~ ":" ~ argType => Arg(name, argType, Nil)}

  def process: Parser[Process] = "process" ~> ident ~ ("(" ~> processArgs <~ ")") ~ opt(":" ~> ident) ~ ("{" ~> processBody <~ "}") ^^ {
    case name ~ args ~ Some(returnType) ~ ((settings, invocation)) => Process(name, args, returnType, settings, invocation)
    case name ~ args ~ None ~ ((settings, invocation)) => Process(name, args, "Signal", settings, invocation)
  }

  def processArgs: Parser[List[Arg]] = repsep(processArg, ",")

  def processArg: Parser[Arg] = argWithModifiers | argWithImplicitType

  def argWithModifiers: Parser[Arg] = ident ~ ":" ~ rep(modifier) ~ ident ^^ {
    case name ~ ":" ~ modifiers ~ argType => Arg(name, argType, modifiers)
  }

  def modifier: Parser[String] = "sticky"

  def argWithImplicitType: Parser[Arg] = ident ^^ {case name => Arg(name, "Signal", Nil)}

  def processBody: Parser[(List[Assignment], FunctionInvocation)] = rep(processSettings) ~ functionInvocation ^^ {
    case settings ~ invocation => (settings, invocation)
  }

  def processSettings: Parser[Assignment] = singleAssignee ~ "=" ~ atomicValue ^^ {
    case assignee ~ "=" ~ expr => Assignment(List(assignee), expr)
  }

  def functionInvocation: Parser[FunctionInvocation] = ident ~ ("(" ~> repsep(ident, ",") <~ ")") ^^ {
    case name ~ args => FunctionInvocation(name, args)
  }

  def assignment: Parser[Assignment] = assignee ~ "=" ~ expr ^^ {
    case assignee ~ "=" ~ expr => Assignment(assignee, expr)
  }

  def assignee: Parser[List[DotNotationAccessor]] = singleAssignee ^^ {case assignee => List(assignee)} | tupledAssignee

  def singleAssignee: Parser[DotNotationAccessor] = rep1sep(ident, ".") ^^ {case parts => DotNotationAccessor(parts)}

  def tupledAssignee: Parser[List[DotNotationAccessor]] = "(" ~> rep1sep(singleAssignee, ",") <~ ")"

  def expr: Parser[Any] = signalInstantiation | processInstantiation | atomicValue

  def signalInstantiation: Parser[Any] = ident ~ "(" ~ repsep(stringLiteral, ",") ~ ")"

  def processInstantiation = ident

  def atomicValue = "true" | "false" | stringLiteral | floatingPointNumber

  def composition = compositionElem ~ "->" ~ rep1sep(compositionElem, "->")

  def compositionElem = ident ~ ":" ~ singleAssignee | ident | "(" ~ rep1sep(ident, ",") ~ ")"
  
  def comment: Parser[String] = "//.*".r ^^ {case _ => ""}

}