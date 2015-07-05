package hdsl.parser

import hdsl.parser.structures._
import hdsl.parser.structures.rhs._
import hdsl.parser.structures.wfelems._

import scala.util.parsing.combinator.JavaTokenParsers

object HdslParser extends JavaTokenParsers {

  def workflow: Parser[List[WfElem]] = rep(workflowElem | forLoop)

  def workflowElem: Parser[WfElem] = signalClass | processClass | assignment | composition | comment

  def signalClass: Parser[SignalClass] = "signal" ~> ident ~ ("(" ~> signalClassArgs <~ ")") ^^ {
    case name ~ args => SignalClass(name, args)
  }

  def signalClassArgs: Parser[List[Arg]] = repsep(signalClassArg, ",") ^^ (List() ++ _)

  def signalClassArg: Parser[Arg] = ident ~ ":" ~ signalClassArgType ^^ { case name ~ ":" ~ argType => Arg(name, argType, Nil)}

  def signalClassArgType: Parser[String] = "String"

  def processClass: Parser[ProcessClass] =
    "process" ~> ident ~ ("(" ~> processClassArgs <~ ")") ~ opt(":" ~> processClassOutArgs) ~ ("{" ~> processBody <~ "}") ^^ {
      case name ~ args ~ Some(returnTypes) ~ ((settings, invocation)) => {
        val processClass = ProcessClass(name, args, returnTypes, invocation)
        settings.foreach(assignment => processClass.setProperty(assignment.lhs.getResolvedParts(), assignment.rhs.asInstanceOf[Expr]))
        processClass
      }
      case name ~ args ~ None ~ ((settings, invocation)) => ProcessClass(name, args, List("Signal"), invocation)
    }

  def processClassArgs: Parser[List[Arg]] = repsep(processClassArg, ",")

  def processClassArg: Parser[Arg] = argWithModifiers | argWithImplicitType

  def processClassOutArgs: Parser[List[String]] = "Unit" ^^ {case _ => List()} |
    ident ^^ {case returnType => List(returnType)} |
    "(" ~> rep1sep(ident, ",") <~ ")" ^^ {
      case returnTypes if returnTypes.contains("Unit") => throw new RuntimeException("Cannot declare 'Unit' as one multiple (tupled) return types of a process")
      case returnTypes => returnTypes
    }

  def argWithModifiers: Parser[Arg] = ident ~ ":" ~ rep(modifier) ~ ident ^^ {
    case name ~ ":" ~ modifiers ~ argType => Arg(name, argType, modifiers)
  }

  def modifier: Parser[String] = "sticky"

  def argWithImplicitType: Parser[Arg] = ident ^^ { case name => Arg(name, "Signal", Nil)}

  def processBody: Parser[(List[WfElemAssignment], String)] = rep(processSettings) ~ processFunction ^^ {
    case settings ~ processFunction => (settings, processFunction)
  }

  def processSettings: Parser[WfElemAssignment] = lhs ~ "=" ~ expr ^^ {
    case assignee ~ "=" ~ value => WfElemAssignment(assignee, value)
  }

  def processFunction: Parser[String] = ident <~ "()"

  def assignment: Parser[Assignment] = "var" ~> ident ~ "=" ~ expr ^^ {
    case varName ~ "=" ~ expr => VarAssignment(varName, expr)
  } |
    lhs ~ "=" ~ rhs ^^ {
      case lhs ~ "=" ~ rhs => WfElemAssignment(lhs, rhs)
    }

  def lhs: Parser[DotNotationAccessor] = rep1sep(ident, ".") ^^ { case parts => DotNotationAccessor(parts)}

  def rhs: Parser[Rhs] = processOrSignalInstantiation | expr

  def processOrSignalInstantiation: Parser[UndefinedInstantiation] = "new" ~> ident ~ ("(" ~> repsep(expr, ",") <~ ")") ~ opt(arrayAccessor) ^^ {
    case className ~ args ~ Some(expr) => UndefinedInstantiation(className, args, expr)
    case className ~ args ~ None => UndefinedInstantiation(className, args, null)
  }

  def expr: Parser[Expr] = "true" ^^ { case _ => Expr(true)} |
    "false" ^^ { case _ => Expr(false)} |
    stringLiteral ^^ { case str => Expr(str)} |
    wholeNumber ^^ { case num => Expr(num.toInt)} |
    floatingPointNumber ^^ { case num => Expr(num.toDouble)} |
    ident ^^ { case varName => Expr(varName)}

  def arrayAccessor: Parser[Expr] = "[" ~> expr <~ "]"

  def composition: Parser[Composition] = compositionElem ~ rep1(conjunctedElem) ^^ {
    case elem ~ conjsAndElems => Composition(elem :: conjsAndElems.map(_._2), conjsAndElems.map(_._1))
  }

  def conjunctedElem: Parser[(Conjunction, CompositionElem)] = conjunction ~ compositionElem ^^ {case conj ~ elem => (conj, elem)}

  def compositionElem: Parser[CompositionElem] = dotNotationPath ~ opt(":" ~> dotNotationPath) ^^ {
    case primaryPath ~ additionalPath => CompositionElem(List(primaryPath), additionalPath.orNull)
  } |
    "(" ~> rep1sep(dotNotationPath, ",") <~ ")" ^^ {
      case primaryPaths => CompositionElem(primaryPaths, null)
    }

  def conjunction: Parser[Conjunction] = "->" ^^ {case _ => Arrow} |
    "-|>" ^^ {case _ => JoinArrow} |
    "-|" ~> wholeNumber <~ ">" ^^ {case num => PartialJoinArrow(num.toInt)} |
    "-||" ~> wholeNumber <~ ">" ^^ {case num => BlockingPartialJoinArrow(num.toInt)}

  def dotNotationPath: Parser[DotNotationAccessor] = ident ~ rep("." ~> ident | arrayAccessor) ^^ {
    case base ~ properties => DotNotationAccessor(base :: properties)
  }

  def forLoop: Parser[ForLoop] = "for" ~> "(" ~> ident ~ opt("," ~> ident) ~ "<-" ~ ident ~ (")" ~> "{" ~> rep(workflowElem) <~ "}") ^^ {
    case loopVar ~ Some(loopIdx) ~ "<-" ~ array ~ wfElems => ForLoop(loopVar, loopIdx, array, wfElems)
    case loopVar ~ None ~ "<-" ~ array ~ wfElems => ForLoop(loopVar, "$index", array, wfElems)
  }

  def comment: Parser[Comment] = "//.*".r ^^ { case text => Comment(text)}

}