package hdsl.parser.structures.rhs

import hdsl.parser.structures.traits.Instantiation

case class SignalInstantiation(className: String, args: List[Expr], arrayAccessor: Expr) extends Rhs with Instantiation {

  def arraylessCopy: SignalInstantiation = SignalInstantiation(className, args, null)

}
