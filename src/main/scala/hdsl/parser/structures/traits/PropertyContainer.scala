package hdsl.parser.structures.traits

import hdsl._
import hdsl.parser.structures.rhs.Atomic

import scala.collection.mutable

trait PropertyContainer {

  private val flatProperties = mutable.Map.empty[List[String], Any]

  def setProperty(path: List[String], rhs: Any) = flatProperties += path -> rhs

  def addAllProperties(sourceContainer: PropertyContainer) = flatProperties ++= sourceContainer.flatProperties

  def resolvedPropertiesMap(/* TODO inject classes, instances, etc. needed for resolution */): MutableMap[String, Any] = {

    def recursivelySetProperty(path: List[String], rhs: Any, targetMap: MutableMap[String, Any]): Unit = {
      path match {
        case List(lastProperty) => rhs match {
          case unresolvedValue: Atomic => targetMap.put(lastProperty, unresolvedValue.evaluate)
          case resolvedValue: Any => targetMap.put(lastProperty, resolvedValue)
        }
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

    val propertiesMap = mutable.Map.empty[String, Any]
    flatProperties foreach {case (path, value) => recursivelySetProperty(path, value, propertiesMap)}
    propertiesMap
  }

}
