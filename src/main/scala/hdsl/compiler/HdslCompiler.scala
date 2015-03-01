package hdsl.compiler

import hdsl.MutableMap
import hdsl.compiler.structures.{Wf, ProcessInstance, SignalInstance}
import hdsl.parser.structures.DotNotationAccessor
import hdsl.parser.structures.rhs.{Expr, ProcessInstantiation, SignalInstantiation}
import hdsl.parser.structures.traits.{Instantiated, Instantiation}
import hdsl.parser.structures.wfelems._

import scala.collection.mutable

class HdslCompiler {

  Wf.init()

  def compile(wfElems: List[WfElem]): MutableMap[String, Any] = {
    Wf.putSignalClass("Signal" -> SignalClass("Signal", Nil))
    val wfElemsAfterFirstPass = HdslCompiler.firstPass(wfElems)
    HdslCompiler.prepareDataStructures(wfElemsAfterFirstPass)
    HdslCompiler.generateOutput()
  }

}

object HdslCompiler {

  /**
   * Maps the wfElems list into a new list where every signal/process array instantiation has its arrayAccessor
   * resolved to an integer (e.g. instead of ReadDataSets[n] it is ReadDataSets[10])
   * No changes to Wf object are performed (although temporary changes may occur, they are cleaned afterwards)
   */
  private def firstPass(wfElems: List[WfElem]): List[WfElem] = {
    val wfElemsAfterFirstPass = wfElems.map({
      case WfElemAssignment(lhs, rhs: SignalInstantiation) if rhs.arrayAccessor != null => {
        val arraySize = rhs.arrayAccessor.evaluate
        if (!arraySize.isInstanceOf[Int]) {
          throw new RuntimeException(s"Cannot declare array of ${rhs.className} as the index expression doesn't evaluate to an integer")
        }
        WfElemAssignment(lhs, SignalInstantiation(rhs.className, rhs.args, Expr(arraySize)))
      }
      case WfElemAssignment(lhs, rhs: ProcessInstantiation) if rhs.arrayAccessor != null => {
        val arraySize = rhs.arrayAccessor.evaluate
        if (!arraySize.isInstanceOf[Int]) {
          throw new RuntimeException(s"Cannot declare array of ${rhs.className} as the index expression doesn't evaluate to an integer")
        }
        WfElemAssignment(lhs, ProcessInstantiation(rhs.className, Expr(arraySize)))
      }
      case VarAssignment(varName, rhs: Expr) => {
        Wf.putVariable(varName -> rhs.evaluate)
        VarAssignment(varName, rhs)
      }
      case x => x
    })
    // clean the variables created for the first pass evaluation
    Wf.variables.clear()
    wfElemsAfterFirstPass
  }

  def prepareDataStructures(wfElems: List[WfElem]): Unit = {

    wfElems.foreach({
      case signalClass: SignalClass => Wf.putSignalClass(signalClass.name -> signalClass)
      case processClass: ProcessClass => Wf.putProcessClass(processClass.name -> processClass)
      case WfElemAssignment(lhs, rhs: Instantiation) => instantiate(lhs, rhs)
      case WfElemAssignment(lhs, rhs: Expr) => setProcessProperty(lhs, rhs)
      case VarAssignment(varName, rhs: Expr) => Wf.putVariable(varName -> rhs.evaluate)
      case c: Composition => c.compose()
      case forLoop: ForLoop => forLoop.execute()
      case c: Comment => "do nothing"
      case x => throw new RuntimeException(s"TODO ($x)")
    })
  }

  private def instantiate(lhs: DotNotationAccessor, instantiation: Instantiation) = {
    val instanceName = lhs match {
      case DotNotationAccessor(List(name: String)) => name
      case x => throw new RuntimeException(s"A signal or process instance cannot be assigned to $x")
    }
    val instance = instantiation.prepareInstance(instanceName)

    if (instantiation.arrayAccessor == null) {
      instance.putInstanceToVisibleAndAll(instanceName)
    } else {
      // create an array signal (to be able to read the array's size later), but don't generate it in output JSON
      Wf.checkNameAvailability(instanceName)
      instance.putInstanceOnlyToVisible(instanceName)

      0 until instantiation.arrayAccessor.value.asInstanceOf[Int] foreach (index => {
        val arrayElemName = DotNotationAccessor(List(instanceName, Expr(index))).stringify
        val arrayElemInstance = instance.instantiation.arraylessCopy.prepareInstance(arrayElemName)
        arrayElemInstance.putInstanceToVisibleAndAll(arrayElemName)
      })
    }
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
