package hdsl.parser.structures.rhs

case class SignalInstantiation(className: String, args: List[Expr], arrayAccessor: Expr) extends AnyRef with Rhs {

}
