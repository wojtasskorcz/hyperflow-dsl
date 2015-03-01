package hdsl.parser.structures.traits

import hdsl.compiler.structures.Wf
import hdsl.parser.structures.DotNotationAccessor
import hdsl.parser.structures.rhs.{Rhs, Expr}

/**
 * Represents an instantiation of a signal/process (creating an instance of the class of the given signal/process and
 * assigning it to a workflow variable)
 */
trait Instantiation extends Rhs {

  def className: String
  def arrayAccessor: Expr
  def changedArrayCopy(newArrayAccessor: Expr): Instantiation
  def prepareInstance(name: String): Instantiated

  def instantiate(lhs: DotNotationAccessor) = {
    val instanceName = lhs match {
      case DotNotationAccessor(List(name: String)) => name
      case x => throw new RuntimeException(s"A signal or process instance cannot be assigned to $x")
    }
    val instance = prepareInstance(instanceName)

    if (arrayAccessor == null) {
      instance.putInstanceToVisibleAndAll(instanceName)
    } else {
      // create an array signal (to be able to read the array's size later), but don't generate it in output JSON
      Wf.checkNameAvailability(instanceName)
      instance.putInstanceOnlyToVisible(instanceName)

      0 until arrayAccessor.value.asInstanceOf[Int] foreach (index => {
        val arrayElemName = DotNotationAccessor(List(instanceName, Expr(index))).stringify
        val arrayElemInstance = instance.instantiation.changedArrayCopy(null).prepareInstance(arrayElemName)
        arrayElemInstance.putInstanceToVisibleAndAll(arrayElemName)
      })
    }
  }

}
