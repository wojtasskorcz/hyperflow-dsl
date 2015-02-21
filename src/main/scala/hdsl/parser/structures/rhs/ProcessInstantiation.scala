package hdsl.parser.structures.rhs

case class ProcessInstantiation(className: String, arrayAccessor: Expr) extends Rhs {

  def arraylessCopy: ProcessInstantiation = ProcessInstantiation(className, null)

}