package hdsl.parser.structures

case class DotNotationAccessor(parts: List[String]) {

  require(parts.length > 0)

  def getBase() = parts(0)

  def getProperties() = parts.drop(1)

}
