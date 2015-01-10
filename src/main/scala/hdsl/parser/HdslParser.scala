package hdsl.parser

import hdsl.parser.structures._
import hdsl.parser.structures.rhs.{Rhs, SignalInstantiation, ProcessInstantiation, Atomic}
import hdsl.parser.structures.wfelems._

import scala.util.parsing.combinator.JavaTokenParsers

object HdslParser extends JavaTokenParsers {
  
  def workflow: Parser[List[WfElem]] = rep(workflowElem) ^^ {case elems => elems filterNot(elem => elem == null)}

  def workflowElem: Parser[WfElem] = signal | process | assignment | composition | comment
  
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

  def processSettings: Parser[Assignment] = singleLhs ~ "=" ~ atomicValue ^^ {
    case assignee ~ "=" ~ value => Assignment(List(assignee), value)
  }

  def functionInvocation: Parser[FunctionInvocation] = ident ~ ("(" ~> repsep(ident, ",") <~ ")") ^^ {
    case name ~ args => FunctionInvocation(name, args)
  }

  def assignment: Parser[Assignment] = lhs ~ "=" ~ rhs ^^ {
    case lhs ~ "=" ~ rhs => Assignment(lhs, rhs)
  }

  def lhs: Parser[List[DotNotationAccessor]] = singleLhs ^^ {case assignee => List(assignee)} | tupledLhs

  def singleLhs: Parser[DotNotationAccessor] = rep1sep(ident, ".") ^^ {case parts => DotNotationAccessor(parts)}

  def tupledLhs: Parser[List[DotNotationAccessor]] = "(" ~> rep1sep(singleLhs, ",") <~ ")"

  def rhs: Parser[Rhs] = signalInstantiation | processInstantiation | atomicValue

  def signalInstantiation: Parser[Rhs] = ident ~ ("(" ~> repsep(atomicValue, ",") <~ ")") ^^ {
    case name ~ args => SignalInstantiation(name, args)
  }

  def processInstantiation: Parser[Rhs] = ident ^^ {case name => ProcessInstantiation(name)}

  def atomicValue: Parser[Rhs] = "true" ^^ {case _ => Atomic(true)} |
    "false" ^^ {case _ => Atomic(false)} |
    stringLiteral ^^ {case str => Atomic(str)} |
    floatingPointNumber ^^ {case num => Atomic(num)}

  def composition: Parser[Composition] = compositionElem ~ "->" ~ rep1sep(compositionElem, "->") ^^ {
    case elem ~ "->" ~ elems => Composition(List(elem) ++ elems)
  }

  def compositionElem: Parser[CompositionElem] = ident ~ opt(":" ~> singleLhs) ^^ {
    case name ~ Some(additional) => CompositionElem(List(name), additional)
    case name ~ None => CompositionElem(List(name), null)
  } |
    "(" ~> rep1sep(ident, ",") <~ ")" ^^ {case names => CompositionElem(names, null)}
  
  def comment: Parser[WfElem] = "//.*".r ^^ {case _ => null}

}