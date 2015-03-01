package hdsl.parser.structures.rhs

import hdsl.compiler.structures.ProcessInstance
import hdsl.parser.structures.traits.Instantiation

case class ProcessInstantiation(className: String, arrayAccessor: Expr) extends Rhs with Instantiation {

  override def arraylessCopy: ProcessInstantiation = ProcessInstantiation(className, null)

  override def prepareInstance(name: String): ProcessInstance = ProcessInstance(name, this)

}