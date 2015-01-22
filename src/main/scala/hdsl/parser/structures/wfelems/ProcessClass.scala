package hdsl.parser.structures.wfelems

import hdsl.MutableMap
import hdsl.parser.structures.rhs.Atomic
import hdsl.parser.structures.{Arg, FunctionInvocation}

import scala.collection.mutable

case class ProcessClass(name: String, args: List[Arg], returnType: String, invocation: FunctionInvocation) extends WfElem {

  val properties = mutable.Map.empty[String, Any]

  def setProperty(path: List[String], rhs: Atomic) = {

    def recursivelySetProperty(path: List[String], rhs: Atomic, targetMap: MutableMap[String, Any]): Unit = {
      path match {
        case List(lastProperty) => targetMap.put(lastProperty, rhs)
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

    recursivelySetProperty(path, rhs, properties)
  }

  def resolvedPropertiesMap(/* TODO classes, instances, etc. needed for resolution */): MutableMap[String, Any] = {

    def recursivelyResolveProperties(map: MutableMap[String, Any]): MutableMap[String, Any] = {
      val outMap = map.map({
        case (s, atomic: Atomic) => (s, atomic.evaluate)
        case (s, innerMap: MutableMap[_, _]) => (s, recursivelyResolveProperties(innerMap.asInstanceOf[MutableMap[String, Any]]))
        case (s, any) => throw new RuntimeException(s"Unexpected $any")
      })
      outMap
    }

    recursivelyResolveProperties(properties)
  }

}
