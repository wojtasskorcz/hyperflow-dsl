package hdsl.compiler

import hdsl.MutableMap
import hdsl.compiler.structures.{ProcessInstance, SignalInstance, WfSpec}
import hdsl.parser.structures.DotNotationAccessor
import hdsl.parser.structures.rhs.{Atomic, ProcessInstantiation, SignalInstantiation}
import hdsl.parser.structures.wfelems._

import scala.collection.mutable

class HdslCompiler {

  val wf = new WfSpec()

  def compile(wfElems: List[WfElem]): MutableMap[String, Any] = {
    prepareDataStructures(wfElems)
    generateOutput()
  }

  private def prepareDataStructures(wfElems: List[WfElem]) = {
    wfElems.foreach({
      case signalClass: SignalClass => wf.putSignalClass(signalClass.name -> signalClass)
      case processClass: ProcessClass => wf.putProcessClass(processClass.name -> processClass)
      case Assignment(lhs, rhs: SignalInstantiation) =>
        wf.putSignalInstance(prepareExplicitSignalInstance(lhs, rhs))
      case Assignment(lhs, rhs: ProcessInstantiation) =>
        wf.putProcessInstance(prepareExplicitProcessInstance(lhs, rhs))
      case Assignment(lhs, rhs: Atomic) => setProcessProperty(lhs, rhs)
      case c: Composition => c.compose(wf)
      case _ => "unimplemented"
    })
  }

  private def prepareExplicitSignalInstance(lhs: DotNotationAccessor, instantiation: SignalInstantiation): (String, SignalInstance) = {
    val signalInstanceName = lhs match {
      case DotNotationAccessor(List(name: String)) => name
      case x => throw new RuntimeException(s"A signal instance cannot be assigned to $x")
    }
    val signalClass = wf.signalClasses.get(instantiation.className) match {
      case Some(signalClass: SignalClass) => signalClass
      case None => throw new RuntimeException(
        s"Cannot instantiate signal $signalInstanceName. Signal class ${instantiation.className} not found")
    }
    (signalInstanceName, SignalInstance(signalInstanceName, signalClass, instantiation))
  }

  private def prepareExplicitProcessInstance(lhs: DotNotationAccessor, instantiation: ProcessInstantiation): (String, ProcessInstance) = {
    val processInstanceName = lhs match {
      case DotNotationAccessor(List(name: String)) => name
      case x => throw new RuntimeException(s"A process instance cannot be assigned to $x")
    }
    val processClass = wf.processClasses.get(instantiation.className) match {
      case Some(processClass: ProcessClass) => processClass
      case None => throw new RuntimeException(
        s"Cannot instantiate process $processInstanceName. Process class ${instantiation.className} not found")
    }
    (processInstanceName, ProcessInstance(processInstanceName, processClass, instantiation))
  }

  private def setProcessProperty(accessor: DotNotationAccessor, rhs: Atomic) = {
    wf.visibleProcessInstances.get(accessor.getBase()) match {
      case Some(processInstance) => processInstance.setProperty(accessor.getProperties(), rhs)
      case None => throw new RuntimeException(
          s"cannot set property ${accessor.getProperties()} as ${accessor.getBase()} is not defined")
    }
  }

  private def generateOutput(): MutableMap[String, Any] = {
    val outMap = mutable.Map.empty[String, Any]
    outMap += "processes" -> wf.allProcessInstances.map(instance => instance.toMap)
    outMap += "signals" -> wf.allSignalInstances.map(instance => instance.toMap)
    outMap
  }

}
