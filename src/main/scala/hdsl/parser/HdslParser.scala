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

  def signalClassArgs: Parser[List[Arg]] = ":" ~> signalClassArgType ^^ { case argType => List(Arg(null, argType, Nil))} |
    repsep(signalClassArg, ",") ^^ (List() ++ _)

  def signalClassArg: Parser[Arg] = ident ~ ":" ~ signalClassArgType ^^ { case name ~ ":" ~ argType => Arg(name, argType, Nil)}

  def signalClassArgType: Parser[String] = "String" | "Array"

  def processClass: Parser[ProcessClass] =
    "process" ~> ident ~ ("(" ~> processClassArgs <~ ")") ~ (":" ~> "(" ~> processClassArgs <~ ")") ~ ("{" ~> processBody <~ "}") ^^ {
      case name ~ args ~ returnTypes ~ ((settings, invocation)) => {
        val processClass = ProcessClass(name, args, returnTypes, invocation)
        settings.foreach(assignment => processClass.setProperty(assignment.lhs.resolvedParts, assignment.rhs))
        processClass
      }
    }

  def processClassArgs: Parser[List[Arg]] = repsep(processClassArg, ",")

  def processClassArg: Parser[Arg] = argWithModifiers | argWithImplicitType

  def argWithModifiers: Parser[Arg] = ident ~ ":" ~ rep(modifier) ~ ident ^^ {
    case name ~ ":" ~ modifiers ~ argType => Arg(name, argType, modifiers)
  }

  def modifier: Parser[String] = "sticky"

  def argWithImplicitType: Parser[Arg] = ident ^^ { case name => Arg(name, "Signal", Nil)}

  def processBody: Parser[(List[WfElemAssignment], String)] = rep(processSettings) ~ processFunction ^^ {
    case settings ~ processFunction => (settings, processFunction)
  }

  def processSettings: Parser[WfElemAssignment] = lhs ~ "=" ~ (expr | exprList) ^^ {
    case assignee ~ "=" ~ value => WfElemAssignment(assignee, value)
  }

  def processFunction: Parser[String] = ident <~ "()"

  def assignment: Parser[Assignment] = "var" ~> ident ~ "=" ~ expr ^^ {
    case varName ~ "=" ~ expr => VarAssignment(varName, expr)
  } |
    dotNotationPath ~ "=" ~ rhs ^^ {
      case lhs ~ "=" ~ rhs => WfElemAssignment(lhs, rhs)
    }

  def lhs: Parser[DotNotationAccessor] = rep1sep(ident, ".") ^^ { case parts => DotNotationAccessor(parts)}

  def rhs: Parser[Rhs] = processOrSignalInstantiation | expr | exprList

  def processOrSignalInstantiation: Parser[UndefinedInstantiation] = "new" ~> ident ~ ("(" ~> repsep(expr | exprList, ",") <~ ")") ~ opt(arrayAccessor) ^^ {
    case className ~ args ~ optExpr => UndefinedInstantiation(className, args, optExpr.orNull)
  }

  def numericExpr: Parser[Expr] = floatingPointNumber ^^ {
      // try to convert to integer; if not possible - convert to double
      case num => try {
        Expr(num.toInt)
      } catch {
        case e: NumberFormatException => Expr(num.toDouble)
      }
    } |
    ident ~ potentialDivision ^^ {
      case varName ~ None => Expr(varName)
      case varName ~ Some(expr) => Expr(List(Expr(varName), expr))
    }

  def expr: Parser[Expr] = "true" ^^ { case _ => Expr(true)} |
    "false" ^^ { case _ => Expr(false)} |
    concatenatedString |
    numericExpr

  def concatenatedString: Parser[Expr] = rep1sep(stringLiteral | dotNotationPath, "+") ^^ {
    case concatenationParts => Expr(concatenationParts)
  }

  // TODO more general logic of arithmetic operations
  def potentialDivision: Parser[Option[Expr]] = opt("/" ~> expr)

  def exprList: Parser[ExprList] = "[" ~> repsep(expr, ",") <~ "]" ^^ { case exprs => ExprList(exprs)}

  def arrayAccessor: Parser[Expr] = "[" ~> numericExpr <~ "]"

  def composition: Parser[Composition] = compositionElem ~ rep1(conjunctedElem) ^^ {
    case elem ~ conjsAndElems => Composition(elem :: conjsAndElems.map(_._2), conjsAndElems.map(_._1))
  }

  def conjunctedElem: Parser[(Conjunction, CompositionElem)] = conjunction ~ compositionElem ^^ {case conj ~ elem => (conj, elem)}

  def compositionElem: Parser[CompositionElem] = dotNotationPath ~ opt(":" ~> (dotNotationPath | wholeNumber)) ^^ {
    case primaryPath ~ None => CompositionElem(List(primaryPath), null)
    case primaryPath ~ Some(accessor: DotNotationAccessor) => CompositionElem(List(primaryPath), Left(accessor))
    case primaryPath ~ Some(num: String) => CompositionElem(List(primaryPath), Right(num.toInt))
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