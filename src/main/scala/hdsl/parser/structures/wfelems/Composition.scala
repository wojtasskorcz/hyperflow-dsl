package hdsl.parser.structures.wfelems

import hdsl.compiler.structures.{ProcessInstance, WfSpec}
import hdsl.parser.structures.CompositionElem
import scala.collection.mutable

case class Composition(elems: List[CompositionElem]) extends WfElem {

  private val tmpProcesses = mutable.Map.empty[String, ProcessInstance]

  def compose(wf: WfSpec) = {
    // for each pair of adjacent composition elements
    elems zip elems.tail foreach {
      case (from, to) if from.isSignalElem(wf) || to.isProcessElem(wf) => setInputs(to, from, wf)
//      case (from, to) if from.isProcessElem(wf) || to.isSignalElem(wf) => setOutputs(from, to, wf)
      case _ => "unimplemented"
    }
  }

  private def setInputs(processElem: CompositionElem, signalElem: CompositionElem, wf: WfSpec) = {
    processElem match {
      case CompositionElem(List(processName), null) => signalElem match {
        case CompositionElem(signalNames, null) => signalNames.foreach(
          signalName => wf.visibleProcessInstances(processName).addInput(wf.visibleSignalInstances(signalName))
        )
      }
    }
  }

}
