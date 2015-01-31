package hdsl.parser.structures.wfelems

import hdsl.compiler.structures.{Wf, SignalInstance, ProcessInstance}
import hdsl.parser.structures.CompositionElem
import hdsl.parser.structures.rhs.SignalInstantiation

import scala.collection.mutable

case class Composition(elems: List[CompositionElem]) extends WfElem {

  private val tmpProcesses = mutable.Map.empty[String, ProcessInstance]

  def compose() = {
    // for each pair of adjacent composition elements
    elems zip elems.tail foreach {
      case (from, to) if from.isSignalElem() || to.isProcessElem() => setInputs(to, from)
      case (from, to) if from.isProcessElem() || to.isSignalElem() => setOutputs(from, to)
      case _ => "unimplemented"
    }
  }

  private def setInputs(processElem: CompositionElem, signalElem: CompositionElem) = {
    processElem match {
      case CompositionElem(List(processName), null) => signalElem match {
        case CompositionElem(signalNames, null) => signalNames.foreach(
          signalName => Wf.visibleProcessInstances(processName).addInput(Wf.visibleSignalInstances(signalName))
        )
      }
    }
  }

  private def setOutputs(processElem: CompositionElem, signalElem: CompositionElem): Unit = {
    val processInstance = processElem match {
      case CompositionElem(List(processName), null) => Wf.visibleProcessInstances.get(processName) match {
        case Some(instance) => instance
        case None => throw new RuntimeException() //wf.processClasses.get(processName) match and create inline if possible
      }
    }

    signalElem match {
      case CompositionElem(signalNames, null) => signalNames.foreach {
        case signalName if Wf.visibleSignalInstances.contains(signalName) => "TODO"
        case signalName => processInstance.addOutput(createOutputSignal(signalName, processInstance))
      }
    }
  }

  private def createOutputSignal(signalName: String, processInstance: ProcessInstance): SignalInstance = {
    val signalClass = processInstance.getOutSignalClass
    if (signalClass.args.nonEmpty) {
      throw new RuntimeException(s"Cannot automatically generate signal $signalName of class ${signalClass.name} because the class takes arguments")
    }
    val signalInstance = SignalInstance(signalName, signalClass, SignalInstantiation(signalClass.name, Nil))
    Wf.putSignalInstance(signalName -> signalInstance)
    signalInstance
  }

}
