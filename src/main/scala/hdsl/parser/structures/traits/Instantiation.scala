package hdsl.parser.structures.traits

import hdsl.parser.structures.rhs.Expr

trait Instantiation {

  def className: String
  def arrayAccessor: Expr
  def arraylessCopy: Instantiation
  def prepareInstance(name: String): Instantiated

}
