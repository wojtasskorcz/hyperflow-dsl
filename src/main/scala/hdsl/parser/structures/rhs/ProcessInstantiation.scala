package hdsl.parser.structures.rhs

import hdsl.compiler.structures.ProcessInstance
import hdsl.parser.structures.traits.Instantiation

case class ProcessInstantiation(className: String, arrayAccessor: Expr) extends Instantiation {

  override def changedArrayCopy(newArrayAccessor: Expr): ProcessInstantiation = ProcessInstantiation(className, newArrayAccessor)

  override def prepareInstance(name: String): ProcessInstance = ProcessInstance(name, this)

}