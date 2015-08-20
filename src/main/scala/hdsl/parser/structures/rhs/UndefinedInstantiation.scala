package hdsl.parser.structures.rhs

/**
 * Used only temporarily by the parser when it's still unknown whether a given expression is a ProcessInstantiation or
 * SignalInstantiation. After resolving by the compile to the appropriate concrete Instantiation class this class is not
 * used anymore.
 */
case class UndefinedInstantiation(className: String, args: List[Rhs], arrayAccessor: Expr) extends Rhs {

}
