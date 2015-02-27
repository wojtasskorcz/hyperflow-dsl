package hdsl.compiler.structures

import hdsl.MutableMap
import hdsl.parser.structures.DotNotationAccessor
import hdsl.parser.structures.rhs.Expr
import hdsl.parser.structures.wfelems.{ProcessClass, SignalClass}

import scala.collection.mutable

object Wf {

  val signalClasses = mutable.Map.empty[String, SignalClass]
  val processClasses = mutable.Map.empty[String, ProcessClass]
  val visibleSignalInstances = mutable.Map.empty[String, SignalInstance]
  val visibleProcessInstances = mutable.Map.empty[String, ProcessInstance]
  val allSignalInstances = mutable.MutableList.empty[SignalInstance]
  val allProcessInstances = mutable.MutableList.empty[ProcessInstance]
  val variables = mutable.Map.empty[String, Any]
  private var anonymousElemCounter = 1

  def init() = {
    signalClasses.clear()
    processClasses.clear()
    visibleSignalInstances.clear()
    visibleProcessInstances.clear()
    allSignalInstances.clear()
    allProcessInstances.clear()
    anonymousElemCounter = 1
  }

  def putSignalClass(elem: (String, SignalClass)) = {
    checkNameAvailability(elem._1)
    signalClasses += elem
  }

  def putProcessClass(elem: (String, ProcessClass)) = {
    checkNameAvailability(elem._1)
    processClasses += elem
  }

  def putSignalInstance(elem: (String, SignalInstance)) = {
    checkNameAvailability(elem._1)
    visibleSignalInstances += elem
    allSignalInstances += elem._2
  }

  def putProcessInstance(elem: (String, ProcessInstance)) = {
    checkNameAvailability(elem._1)
    visibleProcessInstances += elem
    allProcessInstances += elem._2
  }

  def putVariable(elem: (String, Any)) = {
    checkNameAvailability(elem._1)
    variables += elem
  }

  def getNextAnonymousName = {
    val name = "$anonymous" + anonymousElemCounter
    anonymousElemCounter += 1
    name
  }

  /**
   * Variables and Process/Signal instance names can be overwritten, but Process/Signal classes have to stay visible at all times
   */
  def checkNameAvailability(name: String) = {
    if (signalClasses.contains(name) || processClasses.contains(name)) {
      throw new RuntimeException(s"Declaration of $name would overwrite a Signal or Process class definition")
    }
  }

  def forAllCollections(f: mutable.Map[String, _] => Unit) = {
    val collectionsList = List(signalClasses, processClasses, visibleSignalInstances, visibleProcessInstances, variables)
    collectionsList.foreach(f)
  }

}
