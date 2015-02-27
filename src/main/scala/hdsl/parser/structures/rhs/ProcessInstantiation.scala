package hdsl.parser.structures.rhs

import hdsl.parser.structures.traits.Instantiation

case class ProcessInstantiation(className: String, arrayAccessor: Expr) extends Rhs with Instantiation {

  def arraylessCopy: ProcessInstantiation = ProcessInstantiation(className, null)

}