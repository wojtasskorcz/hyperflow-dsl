package hdsl.compiler.structures

import hdsl._
import hdsl.parser.structures.rhs.{Atomic, ProcessInstantiation}
import hdsl.parser.structures.traits.PropertyContainer
import hdsl.parser.structures.wfelems.ProcessClass

import scala.collection.mutable

case class ProcessInstance(name: String, processClass: ProcessClass, instantiation: ProcessInstantiation) extends PropertyContainer {

  def toMap: MutableMap[String, Any] = {
    val outMap = mutable.Map.empty[String, Any]
    outMap += "name" -> name
    outMap += "function" -> processClass.invocation.name
    outMap ++= processClass.resolvedPropertiesMap
    outMap
  }

}
