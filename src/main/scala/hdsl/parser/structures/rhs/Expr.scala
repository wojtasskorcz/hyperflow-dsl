package hdsl.parser.structures.rhs

import hdsl.compiler.structures.Wf

case class Expr(value: Any) extends Rhs {

  def evaluate = value match {
    case s: String if isStringLiteral(s) => s.drop(1).dropRight(1)
    case s: String => {
      if (!Wf.variables.contains(s)) {
        throw new RuntimeException(s"Cannot evaluate expression $value as variable $s is not defined")
      }
      Wf.variables(s)
    }
    case x => x
  }

  private def isStringLiteral(s: String) = s.startsWith("\"") && s.endsWith("\"")

}