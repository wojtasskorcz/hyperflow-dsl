package hdsl.compiler.structures

import hdsl._
import hdsl.parser.structures.rhs.{Atomic, ProcessInstantiation}
import hdsl.parser.structures.wfelems.ProcessClass

import scala.collection.mutable

case class ProcessInstance(name: String, processClass: ProcessClass, instantiation: ProcessInstantiation,
                           properties: MutableMap[String, Any]) {

  def setProperty(path: List[String], rhs: Atomic) = {
    recursivelySetProperty(path, rhs, properties)
  }

  private def recursivelySetProperty(path: List[String], rhs: Atomic, targetMap: MutableMap[String, Any]): Unit = {
    path match {
      case List(lastProperty) => targetMap.put(lastProperty, rhs)
      case longerList => {
        val innerMap = targetMap.get(longerList(0)) match {
          case Some(innerMap: MutableMap[String, Any]) => innerMap
          case _ => {
            val innerMap = mutable.Map.empty[String, Any]
            targetMap.put(path(0), innerMap)
            innerMap
          }
        }
        recursivelySetProperty(path.drop(1), rhs, innerMap)
      }
    }
  }

}
