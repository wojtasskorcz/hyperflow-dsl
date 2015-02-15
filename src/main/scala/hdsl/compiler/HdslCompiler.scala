package hdsl.compiler

import hdsl.MutableMap
import hdsl.compiler.structures.{Wf, ProcessInstance, SignalInstance}
import hdsl.parser.structures.DotNotationAccessor
import hdsl.parser.structures.rhs.{Expr, ProcessInstantiation, SignalInstantiation}
import hdsl.parser.structures.wfelems._

import scala.collection.mutable

class HdslCompiler {

  Wf.init()

  def compile(wfElems: List[WfElem]): MutableMap[String, Any] = {
    prepareDataStructures(wfElems)
    generateOutput()
  }

  private def prepareDataStructures(wfElems: List[WfElem]) = {
    Wf.putSignalClass("Signal" -> SignalClass("Signal", Nil))
    wfElems.foreach({
      case signalClass: SignalClass => Wf.putSignalClass(signalClass.name -> signalClass)
      case processClass: ProcessClass => Wf.putProcessClass(processClass.name -> processClass)
      case WfElemAssignment(lhs, rhs: SignalInstantiation) =>
        Wf.putSignalInstance(prepareExplicitSignalInstance(lhs, rhs))
      case WfElemAssignment(lhs, rhs: ProcessInstantiation) =>
        Wf.putProcessInstance(prepareExplicitProcessInstance(lhs, rhs))
      case WfElemAssignment(lhs, rhs: Expr) => setProcessProperty(lhs, rhs)
      case c: Composition => c.compose()
      case _ => throw new RuntimeException("TODO")
    })
  }

  private def prepareExplicitSignalInstance(lhs: DotNotationAccessor, instantiation: SignalInstantiation): (String, SignalInstance) = {
    val signalInstanceName = lhs match {
      case DotNotationAccessor(List(name: String)) => name
      case x => throw new RuntimeException(s"A signal instance cannot be assigned to $x")
    }
    (signalInstanceName, SignalInstance(signalInstanceName, instantiation))
  }

  private def prepareExplicitProcessInstance(lhs: DotNotationAccessor, instantiation: ProcessInstantiation): (String, ProcessInstance) = {
    val processInstanceName = lhs match {
      case DotNotationAccessor(List(name: String)) => name
      case x => throw new RuntimeException(s"A process instance cannot be assigned to $x")
    }
    (processInstanceName, ProcessInstance(processInstanceName, instantiation))
  }

  private def setProcessProperty(accessor: DotNotationAccessor, rhs: Expr) = {
    Wf.visibleProcessInstances.get(accessor.getBase()) match {
      case Some(processInstance) => processInstance.setProperty(accessor.getResolvedProperties(), rhs)
      case None => throw new RuntimeException(
          s"cannot set property ${accessor.getResolvedProperties()} as ${accessor.getBase()} is not defined")
    }
  }

  private def generateOutput(): MutableMap[String, Any] = {
    val outMap = mutable.Map.empty[String, Any]
    outMap += "processes" -> Wf.allProcessInstances.map(instance => instance.toMap)
    outMap += "signals" -> Wf.allSignalInstances.map(instance => instance.toMap)
    outMap
  }

}
