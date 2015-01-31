package hdsl.parser.structures

import hdsl.compiler.structures.Wf

case class CompositionElem(names: List[String], additional: DotNotationAccessor) {

  def isSignalElem() = {
    names.forall(name => Wf.visibleSignalInstances.contains(name))
//    if (names.forall(name => wf.visibleSignalInstances.contains(name))) {
//      true
//    } else if (names.forall(name => wf.visibleProcessInstances.contains(name))) {
//      false
//    } else {
//      throw new RuntimeException(s"Composition element ($names) cannot be cohesively identified as signals or processes")
//    }
  }

  def isProcessElem() = {
    names.forall(name => Wf.visibleProcessInstances.contains(name))
  }

}
