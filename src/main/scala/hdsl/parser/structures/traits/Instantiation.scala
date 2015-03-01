package hdsl.parser.structures.traits

import hdsl.parser.structures.rhs.{Rhs, Expr}

trait Instantiation extends Rhs {

  def className: String
  def arrayAccessor: Expr
  def changedArrayCopy(newArrayAccessor: Expr): Instantiation
  def prepareInstance(name: String): Instantiated

}
