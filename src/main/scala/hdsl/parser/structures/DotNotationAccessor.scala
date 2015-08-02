package hdsl.parser.structures

import hdsl.parser.structures.rhs.Expr

/**
 * Represents an accessor to signal or process object properties, e.g. 'stationsArr[idx].count' (in the global scope)
 * or 'args.executor' in the process class definition scope. If present, process/signal name and index form the 'base'.
 * Everything after the 'base' is called 'properties'.
 *
 * @param parts List of Strings (for normal dot-separated properties) or Exprs (for array index accessors). One
 *              `parts` list can contain both elements.
 */
case class DotNotationAccessor(parts: List[Any]) {

  require(parts.length > 0)
  require(parts(0).isInstanceOf[String])

  def base: DotNotationAccessor = parts match {
    case List(name: String, idx: Expr, _*) => DotNotationAccessor(List(name, idx))
    case List(name: String, _*) => DotNotationAccessor(List(name))
  }

  def stringifiedBase: String = base.parts match {
    case List(simpleName: String) => simpleName
    case List(name: String, idx: Expr) => s"$name[${idx.evaluate}]"
  }

  def resolvedProperties: List[String] = resolve(parts.drop(base.parts.size))

  def resolvedParts: List[String] = resolve(parts)

  private def resolve(subparts: List[Any]): List[String] = subparts map {
    case s: String => s
    case expr: Expr => expr.evaluate.toString
  }

}
