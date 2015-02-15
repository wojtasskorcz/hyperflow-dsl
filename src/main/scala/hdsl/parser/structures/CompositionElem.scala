package hdsl.parser.structures

import hdsl.MutableMap
import hdsl.compiler.structures.{ProcessInstance, Wf}
import hdsl.parser.structures.rhs.Expr

/**
 * Represents a parsed composition element, which may have one of the forms (list not exhaustive):
 * - 'p' name of an instantiated explicit process or a name of an explcitly or implicitly instantiated signal
 * - 'PartitionData' name of a process class which will be implicitly instantiated
 * - '(xml, config)' tuple containing combinations of the above (all tuple elements have to be either signals or
 * processes)
 * - 'stationsArr[idx]' element of array of signals or processes identified by an index
 * - 'graphArr[idx]:stationsArr[idx].count' same as above but with a count signal from another array of processes
 * - '(dataPartsArr[idx], config)' tuple of indexed signals or processes; each element of `nameIndices` list
 * corresponds to each element of `names`
 */
case class CompositionElem(names: List[String], nameIndices: List[Expr], additional: DotNotationAccessor) {

  def isSignalElem() = {
    names.forall(name => Wf.visibleSignalInstances.contains(name))
  }

  def isProcessElem(tmpProcesses: MutableMap[String, ProcessInstance]) = {
    names.forall(name => Wf.visibleProcessInstances.contains(name) || tmpProcesses.contains(name))
  }

}
