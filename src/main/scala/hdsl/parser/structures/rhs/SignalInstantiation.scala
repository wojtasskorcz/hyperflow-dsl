package hdsl.parser.structures.rhs

import hdsl.compiler.structures.SignalInstance
import hdsl.parser.structures.traits.Instantiation

case class SignalInstantiation(className: String, args: List[Expr], arrayAccessor: Expr) extends Instantiation {

  override def changedArrayCopy(newArrayAccessor: Expr): SignalInstantiation = SignalInstantiation(className, args, newArrayAccessor)

  override def prepareInstance(name: String): SignalInstance = SignalInstance(name, this)

}
