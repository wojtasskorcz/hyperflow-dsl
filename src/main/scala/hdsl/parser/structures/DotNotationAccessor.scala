package hdsl.parser.structures

import hdsl.parser.structures.rhs.Expr

/**
 * Represents an accessor to signal or process object properties, e.g. 'stationsArr[idx].count'
 * @param parts List of Strings (for normal dot-separated properties) or Exprs (for array index accessors). One
 *              `parts` list can contain both elements.
 */
case class DotNotationAccessor(parts: List[Any]) {

  require(parts.length > 0)

  def getBase(): String = {
    if (!parts(0).isInstanceOf[String]) {
      throw new RuntimeException("A dot-notation expression must start with a String, got: " + parts(0))
    }
    parts(0).asInstanceOf[String]
  }

  def getResolvedProperties(): List[String] = {
    parts.drop(1) map {
      case s: String => s
      case expr: Expr => expr.evaluate.toString
    }
  }

  def getResolvedParts(): List[String] = getBase() :: getResolvedProperties()

  def stringify: String = parts match {
    case List(simpleName: String) => simpleName
    case List(name: String, idx: Expr) => s"$name[${idx.evaluate}]"
  }

}
