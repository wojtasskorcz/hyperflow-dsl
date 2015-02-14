package hdsl.parser

import hdsl.parser.structures._
import hdsl.parser.structures.rhs.{Rhs, SignalInstantiation, ProcessInstantiation, Expr}
import hdsl.parser.structures.wfelems._

import scala.util.parsing.combinator.JavaTokenParsers

object HdslParser extends JavaTokenParsers {
  
  def workflow: Parser[List[WfElem]] = rep(workflowElem) ^^ {case elems => elems filterNot(elem => elem == null)}

  def workflowElem: Parser[WfElem] = signalClass | processClass | assignment | composition | comment
  
  def signalClass: Parser[SignalClass] = "signal" ~> ident ~ ("(" ~> signalClassArgs <~ ")") ^^ {
    case name ~ args => SignalClass(name, args)
  }

  def signalClassArgs: Parser[List[Arg]] = repsep(signalClassArg, ",") ^^ (List() ++ _)

  def signalClassArg: Parser[Arg] = ident ~ ":" ~ signalClassArgType ^^ {case name ~ ":" ~ argType => Arg(name, argType, Nil)}

  def signalClassArgType: Parser[String] = "String"

  def processClass: Parser[ProcessClass] =
    "process" ~> ident ~ ("(" ~> processClassArgs <~ ")") ~ opt(":" ~> ident) ~ ("{" ~> processBody <~ "}") ^^ {
    case name ~ args ~ Some(returnType) ~ ((settings, invocation)) => {
      val processClass = ProcessClass(name, args, returnType, invocation)
      settings.foreach(assignment => processClass.setProperty(assignment.lhs.parts, assignment.rhs.asInstanceOf[Expr]))
      processClass
    }
    case name ~ args ~ None ~ ((settings, invocation)) => ProcessClass(name, args, "Signal", invocation)
  }

  def processClassArgs: Parser[List[Arg]] = repsep(processClassArg, ",")

  def processClassArg: Parser[Arg] = argWithModifiers | argWithImplicitType

  def argWithModifiers: Parser[Arg] = ident ~ ":" ~ rep(modifier) ~ ident ^^ {
    case name ~ ":" ~ modifiers ~ argType => Arg(name, argType, modifiers)
  }

  def modifier: Parser[String] = "sticky"

  def argWithImplicitType: Parser[Arg] = ident ^^ {case name => Arg(name, "Signal", Nil)}

  def processBody: Parser[(List[Assignment], FunctionInvocation)] = rep(processSettings) ~ functionInvocation ^^ {
    case settings ~ invocation => (settings, invocation)
  }

  def processSettings: Parser[Assignment] = lhs ~ "=" ~ expr ^^ {
    case assignee ~ "=" ~ value => Assignment(assignee, value)
  }

  def functionInvocation: Parser[FunctionInvocation] = ident ~ ("(" ~> repsep(ident, ",") <~ ")") ^^ {
    case name ~ args => FunctionInvocation(name, args)
  }

  def assignment: Parser[Assignment] = lhs ~ "=" ~ rhs ^^ {
    case lhs ~ "=" ~ rhs => Assignment(lhs, rhs)
  }

  def lhs: Parser[DotNotationAccessor] = rep1sep(ident, ".") ^^ {case parts => DotNotationAccessor(parts)}

  def rhs: Parser[Rhs] = signalInstantiation | processInstantiation | expr

  def signalInstantiation: Parser[SignalInstantiation] = "new" ~> ident ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
    case name ~ args => SignalInstantiation(name, args)
  }

  def processInstantiation: Parser[ProcessInstantiation] = "new" ~> ident ^^ {case name => ProcessInstantiation(name)}

  def expr: Parser[Expr] = "true" ^^ {case _ => Expr(true)} |
    "false" ^^ {case _ => Expr(false)} |
    stringLiteral ^^ {case str => Expr(str)} |
    floatingPointNumber ^^ {case num => Expr(num.toDouble)} |
    ident ^^ {case varName => Expr(varName)}

  def composition: Parser[Composition] = compositionElem ~ "->" ~ rep1sep(compositionElem, "->") ^^ {
    case elem ~ "->" ~ elems => Composition(List(elem) ++ elems)
  }

  def compositionElem: Parser[CompositionElem] = ident ~ opt(":" ~> lhs) ^^ {
    case name ~ Some(additional) => CompositionElem(List(name), additional)
    case name ~ None => CompositionElem(List(name), null)
  } |
    "(" ~> rep1sep(ident, ",") <~ ")" ^^ {case names => CompositionElem(names, null)}
  
  def comment: Parser[WfElem] = "//.*".r ^^ {case _ => null}

}