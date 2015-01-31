package hdsl.parser.structures

import hdsl.compiler.structures.WfSpec

case class CompositionElem(names: List[String], additional: DotNotationAccessor) {

  def isSignalElem(wf: WfSpec) = {
    names.forall(name => wf.visibleSignalInstances.contains(name))
//    if (names.forall(name => wf.visibleSignalInstances.contains(name))) {
//      true
//    } else if (names.forall(name => wf.visibleProcessInstances.contains(name))) {
//      false
//    } else {
//      throw new RuntimeException(s"Composition element ($names) cannot be cohesively identified as signals or processes")
//    }
  }

  def isProcessElem(wf: WfSpec) = {
    names.forall(name => wf.visibleProcessInstances.contains(name))
  }

}
