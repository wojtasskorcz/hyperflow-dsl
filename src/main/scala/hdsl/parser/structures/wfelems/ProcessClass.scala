package hdsl.parser.structures.wfelems

import hdsl.MutableMap
import hdsl.parser.structures.rhs.Atomic
import hdsl.parser.structures.{Arg, FunctionInvocation}

import scala.collection.mutable

case class ProcessClass(name: String, args: List[Arg], returnType: String, settings: List[Assignment],
                   invocation: FunctionInvocation) extends AnyRef with WfElem {

  def settingsMap: MutableMap[String, Any] = {
    val outMap = mutable.Map.empty[String, Any]
    settings.foreach(assignment => recursivelySetProperty(assignment.lhs.parts, assignment.rhs.asInstanceOf[Atomic], outMap))
    outMap
  }

  private def recursivelySetProperty(path: List[String], rhs: Atomic, targetMap: MutableMap[String, Any]): Unit = {
    path match {
      case List(lastProperty) => targetMap.put(lastProperty, rhs.evaluate)
      case longerList => {
        val innerMap = targetMap.get(longerList(0)) match {
          case Some(innerMap: MutableMap[_, _]) => innerMap.asInstanceOf[MutableMap[String, Any]]
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
