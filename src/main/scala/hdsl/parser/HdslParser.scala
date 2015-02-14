package hdsl.parser

import hdsl.parser.structures._
import hdsl.parser.structures.rhs.{Rhs, SignalInstantiation, ProcessInstantiation, Expr}
import hdsl.parser.structures.wfelems._

import scala.util.parsing.combinator.JavaTokenParsers

object HdslParser extends JavaTokenParsers {
  
  def workflow: Parser[List[WfElem]] = rep(workflowElem | forLoop) ^^ {case elems => elems filterNot(elem => elem == null)}

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

  def processBody: Parser[(List[WfElemAssignment], FunctionInvocation)] = rep(processSettings) ~ functionInvocation ^^ {
    case settings ~ invocation => (settings, invocation)
  }

  def processSettings: Parser[WfElemAssignment] = lhs ~ "=" ~ expr ^^ {
    case assignee ~ "=" ~ value => WfElemAssignment(assignee, value)
  }

  def functionInvocation: Parser[FunctionInvocation] = ident ~ ("(" ~> repsep(ident, ",") <~ ")") ^^ {
    case name ~ args => FunctionInvocation(name, args)
  }

  def assignment: Parser[Assignment] = "var" ~> ident ~ "=" ~ expr ^^ {
        case varName ~ "=" ~ expr => VarAssignment(varName, expr)
      } |
      lhs ~ "=" ~ rhs ^^ {
          case lhs ~ "=" ~ rhs => WfElemAssignment(lhs, rhs)
    }

  def lhs: Parser[DotNotationAccessor] = rep1sep(ident, ".") ^^ {case parts => DotNotationAccessor(parts)}

  def rhs: Parser[Rhs] = signalInstantiation | processInstantiation | expr

  def signalInstantiation: Parser[SignalInstantiation] = "new" ~> ident ~ ("(" ~> repsep(expr, ",") <~ ")") ~ opt(arrayAccessor) ^^ {
    case name ~ args ~ Some(expr) => SignalInstantiation(name, args, expr)
    case name ~ args ~ None => SignalInstantiation(name, args, null)
  }

  def processInstantiation: Parser[ProcessInstantiation] = "new" ~> ident ~ opt(arrayAccessor) ^^ {
    case name ~ Some(expr) => ProcessInstantiation(name, expr)
    case name ~ None => ProcessInstantiation(name, null)
  }

  def expr: Parser[Expr] = "true" ^^ {case _ => Expr(true)} |
    "false" ^^ {case _ => Expr(false)} |
    stringLiteral ^^ {case str => Expr(str)} |
    floatingPointNumber ^^ {case num => Expr(num.toDouble)} |
    ident ^^ {case varName => Expr(varName)}

  def arrayAccessor: Parser[Expr] = "[" ~> expr <~ "]"

  def composition: Parser[Composition] = compositionElem ~ "->" ~ rep1sep(compositionElem, "->") ^^ {
    case elem ~ "->" ~ elems => Composition(List(elem) ++ elems)
  }

  def compositionElem: Parser[CompositionElem] = ident ~ opt(arrayAccessor) ~ opt(":" ~> lhs) ^^ {
    case name ~ idx ~ additional => CompositionElem(List(name), List(idx.orNull), additional.orNull)
  } |
    "(" ~> rep1sep(ident, ",") <~ ")" ^^ {case names => CompositionElem(names, List.fill(names.length)(null), null)}

  def forLoop: Parser[ForLoop] = "for" ~> "(" ~> ident ~ opt("," ~> ident) ~ "<-" ~ ident ~ (")" ~> "{" ~> rep(workflowElem) <~ "}") ^^ {
    case loopVar ~ Some(loopIdx) ~ "<-" ~ array ~ wfElems => ForLoop(loopVar, loopIdx, array, wfElems)
    case loopVar ~ None ~ "<-" ~ array ~ wfElems => ForLoop(loopVar, null, array, wfElems)
  }
  
  def comment: Parser[WfElem] = "//.*".r ^^ {case _ => null}

}