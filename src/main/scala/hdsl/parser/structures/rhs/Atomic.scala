package hdsl.parser.structures.rhs

case class Atomic(value: Any) extends AnyRef with Rhs {

  def evaluate = value match {
    case s: String if isStringLiteral(s) => s.drop(1).dropRight(1)
    case x => x
  }

  private def isStringLiteral(s: String) = s.startsWith("\"") && s.endsWith("\"")

}