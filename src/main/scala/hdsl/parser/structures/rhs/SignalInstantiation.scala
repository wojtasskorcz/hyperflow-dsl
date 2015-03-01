package hdsl.parser.structures.rhs

import hdsl.compiler.structures.SignalInstance
import hdsl.parser.structures.traits.Instantiation

case class SignalInstantiation(className: String, args: List[Expr], arrayAccessor: Expr) extends Rhs with Instantiation {

  override def arraylessCopy: SignalInstantiation = SignalInstantiation(className, args, null)

  override def prepareInstance(name: String): SignalInstance = SignalInstance(name, this)

}
