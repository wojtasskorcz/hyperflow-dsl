package hdsl.compiler

import hdsl.MutableMap
import hdsl.compiler.structures.{ProcessInstance, SignalInstance}
import hdsl.parser.structures.DotNotationAccessor
import hdsl.parser.structures.rhs.{Atomic, ProcessInstantiation, SignalInstantiation}
import hdsl.parser.structures.wfelems.{Assignment, ProcessClass, SignalClass, WfElem}

import scala.collection.mutable

class HdslCompiler {

  val usedIdentifiers = mutable.Set.empty[String]
  val signalClasses = mutable.Map.empty[String, SignalClass]
  val processClasses = mutable.Map.empty[String, ProcessClass]
  val signalInstances = mutable.Map.empty[String, SignalInstance]
  val processInstances = mutable.Map.empty[String, ProcessInstance]

  def compile(wfElems: List[WfElem]): MutableMap[String, Any] = {
    prepareDataStructures(wfElems)
    generateOutput()
  }

  def prepareDataStructures(wfElems: List[WfElem]) = {
    wfElems.foreach({
      case signalClass: SignalClass => putUnique(signalClasses, signalClass.name -> signalClass)
      case processClass: ProcessClass => putUnique(processClasses, processClass.name -> processClass)
      case Assignment(lhs, rhs: SignalInstantiation) =>
        putUnique(signalInstances, prepareExplicitSignalInstance(lhs, rhs))
      case Assignment(lhs, rhs: ProcessInstantiation) =>
        putUnique(processInstances, prepareExplicitProcessInstance(lhs, rhs))
      case Assignment(lhs, rhs: Atomic) => setProcessProperty(lhs, rhs)
      case _ => "unimplemented"
    })
  }

  def putUnique[V](map: MutableMap[String, V], elem: (String, V)) = {
    val key = elem._1
    if (usedIdentifiers.contains(key)) {
      throw new RuntimeException(s"Declaration of $key is not unique")
    }
    usedIdentifiers += key
    map += elem
  }

  def prepareExplicitSignalInstance(lhs: DotNotationAccessor, instantiation: SignalInstantiation): (String, SignalInstance) = {
    val signalInstanceName = lhs match {
      case DotNotationAccessor(List(name: String)) => name
      case x => throw new RuntimeException(s"A signal instance cannot be assigned to $x")
    }
    val signalClass = signalClasses.get(instantiation.className) match {
      case Some(signalClass: SignalClass) => signalClass
      case None => throw new RuntimeException(
        s"Cannot instantiate signal $signalInstanceName. Signal class ${instantiation.className} not found")
    }
    (signalInstanceName, SignalInstance(signalInstanceName, signalClass, instantiation))
  }

  def prepareExplicitProcessInstance(lhs: DotNotationAccessor, instantiation: ProcessInstantiation): (String, ProcessInstance) = {
    val processInstanceName = lhs match {
      case DotNotationAccessor(List(name: String)) => name
      case x => throw new RuntimeException(s"A process instance cannot be assigned to $x")
    }
    val processClass = processClasses.get(instantiation.className) match {
      case Some(processClass: ProcessClass) => processClass
      case None => throw new RuntimeException(
        s"Cannot instantiate process $processInstanceName. Process class ${instantiation.className} not found")
    }
    (processInstanceName, ProcessInstance(processInstanceName, processClass, instantiation))
  }

  def setProcessProperty(accessor: DotNotationAccessor, rhs: Atomic) = {
    processInstances.get(accessor.getBase()) match {
      case Some(processInstance) => processInstance.setProperty(accessor.getProperties(), rhs)
      case None => throw new RuntimeException(
          s"cannot set property ${accessor.getProperties()} as ${accessor.getBase()} is not defined")
    }
  }

  def generateOutput(): MutableMap[String, Any] = {
    val outMap = mutable.Map.empty[String, Any]
    outMap += "processes" -> processInstances.values.map(instance => instance.toMap)
    outMap
  }

}
