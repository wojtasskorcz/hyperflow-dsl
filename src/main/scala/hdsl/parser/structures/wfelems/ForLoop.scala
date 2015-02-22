package hdsl.parser.structures.wfelems

import hdsl.MutableMap
import hdsl.compiler.HdslCompiler
import hdsl.compiler.structures.{ProcessInstance, SignalInstance, Wf}

import scala.collection.mutable

case class ForLoop(loopVar: String, loopIdx: String, array: String, wfElems: List[WfElem]) extends WfElem {

  require(loopVar != null && loopIdx != null)

  private val varsBackup = mutable.MutableList.empty[(MutableMap[String, _], String, Any)]

  def execute() = {
    println("Before creating vars:\n" + Wf.visibleSignalInstances + "\n" + Wf.visibleProcessInstances + "\n" + Wf
      .variables)
    createTemporaryLoopVar()
    createTemporaryLoopIdx()
    println("With loop vars:\n" + Wf.visibleSignalInstances + "\n" + Wf.visibleProcessInstances + "\n" + Wf.variables)
    HdslCompiler.prepareDataStructures(wfElems)
    restoreBackedUpVars()
    println("After restoring vars:\n" + Wf.visibleSignalInstances + "\n" + Wf.visibleProcessInstances + "\n" + Wf
      .variables)
  }

  private def createTemporaryLoopVar() = {
    Wf.checkNameAvailability(loopVar)
    backupVar(loopVar)
    if (Wf.visibleSignalInstances.contains(array)) {
      val arrayInstance = Wf.visibleSignalInstances(array)
      if (arrayInstance.instantiation.arrayAccessor == null) {
        throw new RuntimeException(s"Cannot iterate over $array as it is not an array")
      }
      val loopVarInstance = Wf.visibleSignalInstances(s"$array[0]")
      Wf.visibleSignalInstances += loopVar -> loopVarInstance
    } else if (Wf.visibleProcessInstances.contains(array)) {
      val arrayInstance = Wf.visibleProcessInstances(array)
      if (arrayInstance.instantiation.arrayAccessor == null) {
        throw new RuntimeException(s"Cannot iterate over $array as it is not an array")
      }
      val loopVarInstance = Wf.visibleProcessInstances(s"$array[0]")
      Wf.visibleProcessInstances += loopVar -> loopVarInstance
    } else {
      throw new RuntimeException(s"Cannot iterate over $array as it is neither signal nor process array")
    }
  }

  private def createTemporaryLoopIdx() = {
    Wf.checkNameAvailability(loopIdx)
    backupVar(loopIdx)
    Wf.putVariable(loopIdx -> 0)
  }

  private def backupVar(varName: String) = {
    Wf.forAllCollections(collection => {
      if (collection.contains(varName)) {
        varsBackup += ((collection, varName, collection(varName)))
        collection -= varName
      }
    })
  }

  private def restoreBackedUpVars() = {
    Wf.forAllCollections(collection => {
      if (collection.contains(loopVar)) {
        collection -= loopVar
      }
      if (collection.contains(loopIdx)) {
        collection -= loopIdx
      }
    })
    varsBackup foreach {
      case (collection, varName, varValue) => {
        collection.asInstanceOf[MutableMap[String, Any]].put(varName, varValue)
      }
    }
  }

}
