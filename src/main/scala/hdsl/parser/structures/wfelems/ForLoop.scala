package hdsl.parser.structures.wfelems

import hdsl.MutableMap
import hdsl.compiler.HdslCompiler
import hdsl.compiler.structures.{ProcessInstance, SignalInstance, Wf}
import hdsl.parser.structures.traits.Instantiated

import scala.collection.mutable

case class ForLoop(loopVar: String, loopIdx: String, array: String, wfElems: List[WfElem]) extends WfElem {

  require(loopVar != null && loopIdx != null)

  /**
   * A list of variables (SignalInstances, ProcessInstances and Variables) backed up before entering the loop. The
   * variables are stored as a tuple:
   * (Wf collection originally containing the variable, variable name, variable value)
   */
  private val varsBackup = mutable.MutableList.empty[(MutableMap[String, _], String, Any)]

  def execute() = {
    Wf.checkNameAvailability(loopVar)
    Wf.checkNameAvailability(loopIdx)
    backupVar(loopVar)
    backupVar(loopIdx)

    if (Wf.visibleSignalInstances.contains(array)) {
      loopOverArray(Wf.visibleSignalInstances)
    } else if (Wf.visibleProcessInstances.contains(array)) {
      loopOverArray(Wf.visibleProcessInstances)
    } else {
      throw new RuntimeException(s"Cannot iterate over $array as it is neither signal nor process array")
    }

    restoreBackedUpVars()
  }

  /**
   * Loops over the loop's `array` using `typeMap` collection (either Wf.visibleSignalInstances or Wf
   * .visibleProcessInstances depending on the type of `array`) to store temporary variables. With each iteration all
   * WfElems contained within the loop are compiled.
   * @param typeMap Wf.visibleSignalInstnaces (if Wf.visibleSignalInstances.contains(array)) or Wf
   *                .visibleProcessInstances (if Wf.visibleProcessInstances.contains(array))
   * @tparam T either SignalInstance or ProcessInstance determined as described above
   */
  private def loopOverArray[T <: Instantiated](typeMap: MutableMap[String, T]) = {
    val arrayInstance = typeMap(array)
    if (arrayInstance.instantiation.arrayAccessor == null) {
      throw new RuntimeException(s"Cannot iterate over $array as it is not an array")
    }
    val arraySize = arrayInstance.instantiation.arrayAccessor.value.asInstanceOf[Int]
    0 until arraySize foreach (index => {
      val loopVarInstance = typeMap(s"$array[$index]")
      typeMap += loopVar -> loopVarInstance
      Wf.variables += loopIdx -> index
      Wf.variables += "$loopIdxVar" -> loopIdx
      Wf.variables += "$arraySize" -> arraySize
      HdslCompiler.secondPass(wfElems)
    })
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
    Wf.variables -= "$loopIdxVar"
    Wf.variables -= "$arraySize"
  }

}
