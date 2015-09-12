package hdsl.parser.structures.rhs

import hdsl.compiler.structures.Wf
import hdsl.parser.structures.DotNotationAccessor

case class Expr(value: Any) extends Rhs {

  def evaluate: Any = value match {
    case s: String => {
      if (!Wf.variables.contains(s)) {
        throw new RuntimeException(s"Cannot evaluate expression $value as variable $s is not defined")
      }
      Wf.variables(s)
    }
    // TODO more general logic of arithmetic operations
    case List(exp1: Expr, exp2: Expr) => exp1.evaluate.asInstanceOf[Int] / exp2.evaluate.asInstanceOf[Int]
    case concatenationParts: List[Any] => evaluateConcatenatedString(concatenationParts)
    case x => x
  }

  private def evaluateConcatenatedString(parts: List[Any]): String = {
    parts.foldLeft("")((res: String, elem) => elem match {
      case s: String if isStringLiteral(s) => res + s.drop(1).dropRight(1)
      case accessor: DotNotationAccessor => res + accessor.evaluate.toString
    })
  }

  private def isStringLiteral(s: String) = s.startsWith("\"") && s.endsWith("\"")

}