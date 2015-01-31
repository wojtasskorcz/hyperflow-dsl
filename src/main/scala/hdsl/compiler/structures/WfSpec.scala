package hdsl.compiler.structures

import hdsl._
import hdsl.parser.structures.wfelems.{Composition, ProcessClass, SignalClass}

import scala.collection.mutable

class WfSpec {

  val signalClasses = mutable.Map.empty[String, SignalClass]
  val processClasses = mutable.Map.empty[String, ProcessClass]
  val visibleSignalInstances = mutable.Map.empty[String, SignalInstance]
  val visibleProcessInstances = mutable.Map.empty[String, ProcessInstance]
  val allSignalInstances = mutable.MutableList.empty[SignalInstance]
  val allProcessInstances = mutable.MutableList.empty[ProcessInstance]
  val compositions = mutable.Map.empty[String, Composition]

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

  private def checkNameAvailability(name: String) = {
    if (signalClasses.contains(name) || processClasses.contains(name)) {
      throw new RuntimeException(s"Declaration of $name would overwrite a Signal or Process class definition")
    }
  }

}
