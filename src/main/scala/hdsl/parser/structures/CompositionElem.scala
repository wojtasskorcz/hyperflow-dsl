package hdsl.parser.structures

import hdsl.MutableMap
import hdsl.compiler.structures.{ProcessInstance, Wf}
import hdsl.parser.structures.rhs.Expr

case class CompositionElem(names: List[String], nameIndices: List[Expr], additional: DotNotationAccessor) {

  def isSignalElem() = {
    names.forall(name => Wf.visibleSignalInstances.contains(name))
  }

  def isProcessElem(tmpProcesses: MutableMap[String, ProcessInstance]) = {
    names.forall(name => Wf.visibleProcessInstances.contains(name) || tmpProcesses.contains(name))
  }

}
