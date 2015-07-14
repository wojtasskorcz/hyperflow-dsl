package hdsl.parser.structures.traits

import hdsl._
import hdsl.parser.structures.rhs.{Rhs, ExprList, Expr}

import scala.collection.mutable

trait PropertyContainer {

  private val flatProperties = mutable.Map.empty[List[String], Any]

  def setProperty(path: List[String], rhs: Rhs) = flatProperties += path -> rhs

  def addAllProperties(sourceContainer: PropertyContainer) = flatProperties ++= sourceContainer.flatProperties

  def resolvedPropertiesMap(): MutableMap[String, Any] = {

    def recursivelySetProperty(path: List[String], rhs: Any, targetMap: MutableMap[String, Any]): Unit = {
      path match {
        case List(lastProperty) => rhs match {
          case expr: Expr => targetMap.put(lastProperty, expr.evaluate)
          case exprList: ExprList => targetMap.put(lastProperty, exprList.parts.map(expr => expr.evaluate))
          case otherValue: Any => throw new RuntimeException("Only Expr or List[Expr] should be stored as properties")
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
    flatProperties foreach { case (path, value) => recursivelySetProperty(path, value, propertiesMap)}
    propertiesMap
  }

}
