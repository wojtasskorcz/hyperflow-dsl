package hdsl.compiler

import hdsl.MutableMap
import hdsl.compiler.structures.{ProcessInstance, SignalInstance, Wf}
import hdsl.parser.structures.DotNotationAccessor
import hdsl.parser.structures.rhs.{ProcessInstantiation, UndefinedInstantiation, SignalInstantiation, Expr}
import hdsl.parser.structures.traits.Instantiation
import hdsl.parser.structures.wfelems._

import scala.collection.mutable

object HdslCompiler {

  val countSignalClassName = "$ControlCount"
  val mergeSignalClassName = "$ControlMerge"
  val nextSignalClassName = "$ControlNext"

  def compile(wfElems: List[WfElem]): MutableMap[String, Any] = {
    val wfElemsAfterFirstPass = firstPass(wfElems)
    createPredefs()
    secondPass(wfElemsAfterFirstPass)
    thirdPass()
    generateOutput()
  }

  private def createPredefs() = {
    Wf.putSignalClass("Signal" -> SignalClass("Signal", Nil))

    val countSignalClass = SignalClass(countSignalClassName, Nil)
    countSignalClass.control = Some("count")
    Wf.putSignalClass(countSignalClassName -> countSignalClass)

    val mergeSignalClass = SignalClass(mergeSignalClassName, Nil)
    mergeSignalClass.control = Some("merge")
    Wf.putSignalClass(mergeSignalClassName -> mergeSignalClass)

    val nextSignalClass = SignalClass(nextSignalClassName, Nil)
    nextSignalClass.control = Some("next")
    Wf.putSignalClass(nextSignalClassName -> nextSignalClass)
  }

  /**
   * Maps the wfElems list into a new list where every signal/process array instantiation has its arrayAccessor
   * resolved to an integer (e.g. instead of ReadDataSets[n] it is ReadDataSets[10])
   * Also, resolves UndefinedInstantiations to be either SignalInstantiations or ProcessInstantiations
   * No changes to Wf object are performed (although temporary changes may occur, they are cleaned afterwards)
   */
  private def firstPass(wfElems: List[WfElem]): List[WfElem] = {
    val wfElemsAfterFirstPass = wfElems.map({
      case WfElemAssignment(lhs, UndefinedInstantiation(className, args, arrayAccessor)) => {
        val arraySize = arrayAccessor match {
          case null => null
          case expr => {
            val size = expr.evaluate
            if (!size.isInstanceOf[Int]) {
              throw new RuntimeException(s"Cannot declare array of $className as the index expression doesn't evaluate to an integer")
            }
            Expr(size)
          }
        }
        val newRhs =
          if (Wf.signalClasses contains className) SignalInstantiation(className, args, arraySize)
          else if (Wf.processClasses contains className) ProcessInstantiation(className, arraySize)
          else throw new RuntimeException(s"Cannot resolve instantiation of $className as neither Signal nor Process")

        WfElemAssignment(lhs, newRhs)
      }
      case VarAssignment(varName, rhs: Expr) => {
        Wf.putVariable(varName -> rhs.evaluate)
        VarAssignment(varName, rhs)
      }
      case signalClass: SignalClass => {
        Wf.putSignalClass(signalClass.name -> signalClass)
        signalClass
      }
      case processClass: ProcessClass => {
        Wf.putProcessClass(processClass.name -> processClass)
        processClass
      }
      case x => x
    })
    // clean the variables created for the first pass evaluation
    Wf.variables.clear()
    Wf.signalClasses.clear()
    Wf.processClasses.clear()
    wfElemsAfterFirstPass
  }

  def secondPass(wfElems: List[WfElem]): Unit = {

    def setProcessProperty(accessor: DotNotationAccessor, rhs: Expr) = {
      Wf.visibleProcessInstances.get(accessor.getBase()) match {
        case Some(processInstance) => processInstance.setProperty(accessor.getResolvedProperties(), rhs)
        case None => throw new RuntimeException(
          s"cannot set property ${accessor.getResolvedProperties()} as ${accessor.getBase()} is not defined")
      }
    }

    wfElems.foreach({
      case signalClass: SignalClass => Wf.putSignalClass(signalClass.name -> signalClass)
      case processClass: ProcessClass => Wf.putProcessClass(processClass.name -> processClass)
      case WfElemAssignment(lhs, rhs: Instantiation) => rhs.instantiate(lhs)
      case WfElemAssignment(lhs, rhs: Expr) => setProcessProperty(lhs, rhs)
      case VarAssignment(varName, rhs: Expr) => Wf.putVariable(varName -> rhs.evaluate)
      case c: Composition => c.compose()
      case forLoop: ForLoop => forLoop.execute()
      case c: Comment => "do nothing"
      case x => throw new RuntimeException(s"TODO ($x)")
    })
  }

  private def thirdPass(): Unit = {

    def connectWithControlSignal(from: ProcessInstance, to: ProcessInstance, signalClassName: String): Unit = {
      val signal = SignalInstance(Wf.getNextAnonymousName, SignalInstantiation(signalClassName, Nil, null))
      Wf.allSignalInstances += signal
      from.addOutput(signal)
      to.addInput(signal)
    }

    Wf.allProcessInstances.filter(_.processType == "join").foreach(processInstance => {
      processInstance.joinCount match {
        case Some(num) => {
          processInstance.computeActiveBranchesCount()
          if (processInstance.isBlockingJoin) {
            connectWithControlSignal(processInstance, processInstance.choiceSource.get, nextSignalClassName)
          }
        }
        case None => connectWithControlSignal(processInstance.choiceSource.get, processInstance, mergeSignalClassName)
      }
    })
  }

  private def generateOutput(): MutableMap[String, Any] = {
    val outMap = mutable.Map.empty[String, Any]
    outMap += "processes" -> Wf.allProcessInstances.map(instance => instance.toMap)
    outMap += "signals" -> Wf.allSignalInstances.map(instance => instance.toMap)
    outMap
  }

}
