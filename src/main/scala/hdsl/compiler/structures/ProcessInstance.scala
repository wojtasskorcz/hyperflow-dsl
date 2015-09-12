package hdsl.compiler.structures

import hdsl._
import hdsl.parser.structures.Arg
import hdsl.parser.structures.rhs.ProcessInstantiation
import hdsl.parser.structures.traits.Instantiated
import hdsl.parser.structures.wfelems.{ProcessClass, SignalClass}

import scala.collection.mutable

/**
 * `name` is the name of the instance that will be set in generated JSON (may be sth like `anonymous$12` or `pArr$0[3]`
 * it is NOT the name of the identifier (variable) this process instance is assigned to
 */
case class ProcessInstance(name: String, instantiation: ProcessInstantiation) extends Instantiated {

  final val processClass = Wf.processClasses.get(instantiation.className) match {
    case Some(processClass: ProcessClass) => processClass
    case None => throw new RuntimeException(
      s"Cannot instantiate process $name. Process class ${instantiation.className} not found")
  }

  addAllProperties(processClass)

  val ins = mutable.MutableList.empty[(SignalInstance, String)]
  val outs = mutable.MutableList.empty[(SignalInstance, String)]
  val sticky = mutable.MutableList.empty[String]
  var choiceSource: Option[ProcessInstance] = None
  var processType = "dataflow"
  var joinCount: Option[Int] = None
  var isBlockingJoin: Boolean = false
  var activeBranchesCount: Option[Int] = None

  override def putInstanceOnlyToVisible(visibleName: String): Unit =  Wf.visibleProcessInstances += visibleName -> this

  override def putInstanceToVisibleAndAll(visibleName: String): Unit =  Wf.putProcessInstance(visibleName -> this)

  def toMap: MutableMap[String, Any] = {
    val outMap = mutable.Map[String, Any]("name" -> name, "function" -> processClass.function)
    outMap ++= resolvedPropertiesMap()
    outMap += "type" -> processType
    outMap += "ins" -> ProcessInstance.signalsToJson(ins)
    outMap += "outs" -> ProcessInstance.signalsToJson(outs)
    if (sticky.nonEmpty) outMap += "sticky" -> sticky
    if (joinCount.nonEmpty) {
      outMap += "joinCount" -> joinCount.get
      outMap += "activeBranchesCount" -> activeBranchesCount.get
    }
    outMap
  }

  def addInput(signal: SignalInstance, suffix: String = "") = {
    // we don't check all the preconditions if it's the workflow's 'ins'
    if (name != "workflow") {
      // if it's not a control signal, we have to check if it adheres to the process class signature
      if (signal.signalClass.control.isEmpty) {
        if (ins.size == processClass.args.size) {
          throw new RuntimeException(s"Cannot add signal ${signal.name} as input to process $name. Too many inputs.")
        }
        val inputArg = processClass.args(ins.size)
        if (signal.signalClass.name != inputArg.argType) {
          throw new RuntimeException(s"Input number ${ins.size} of process $name is of type ${inputArg.argType} cannot be set to signal ${signal.name} of class ${signal.signalClass.name}")
        }
        if (inputArg.modifiers.contains("sticky")) {
          sticky += signal.name
        }
      }
      if (signal.choiceSource.nonEmpty) {
        processType match {
          case "join" => choiceSource = signal.choiceSource
          case _ => choiceSource match {
            case None => processClass.returnTypes.size match {
              case 1 => choiceSource = signal.choiceSource
              case n => throw new RuntimeException(s"Cannot set signal ${signal.name} deriving from a choice process as input to process $name as the process doesn't have exactly one output")
            }
            case Some(alreadySetChoiceSource) => throw new RuntimeException(s"Cannot set two signals deriving from a choice process as inputs to the same process $name. Conflicting signal: ${signal.name}")
          }
        }
      }
    }
    ins += ((signal, suffix))
  }

  def addOutput(signal: SignalInstance) = {
    // we don't check all the preconditions if it's the workflow's 'outs'
    if (name != "workflow") {
      if (outs.size >= processClass.returnTypes.size && signal.signalClass.control.isEmpty) {
        throw new RuntimeException(s"Cannot add another output signal ${signal.name} to process $name")
      }
      if (choiceSource.nonEmpty && processType != "join") {
        signal.choiceSource = choiceSource
      }
    }
    outs += ((signal, ""))
  }

  def getNextOutSignalClass: Option[SignalClass] =
    if (name == "workflow") Wf.signalClasses.get("Signal")
    else if (outs.size >= processClass.returnTypes.size) None
    else Some(Wf.signalClasses(processClass.returnTypes(outs.size).argType))

  def getNextInSignalClass: Option[SignalClass] =
    if (name == "workflow") Wf.signalClasses.get("Signal")
    else if (ins.size >= processClass.args.size) None
    else Some(Wf.signalClasses(processClass.args(ins.size).argType))

  def computeActiveBranchesCount() = {
    activeBranchesCount = Some(Wf.allSignalInstances.count(
      signalInstance => ins.exists { case (signal, suffix) => signal.name == signalInstance.name } && signalInstance.choiceSource == choiceSource))
  }

  override def getProperty(propertyName: String): Option[Any] = {
    def getAttachedSignal(args: List[Arg], signals: MutableList[(SignalInstance, String)], propertyName: String): Option[SignalInstance] =
      args.indexWhere(arg => arg.name == propertyName) match {
        case -1 => None
        case idx => Some(signals(idx)._1)
      }

    super.getProperty(propertyName).orElse(getAttachedSignal(processClass.args, ins, propertyName))
      .orElse(getAttachedSignal(processClass.returnTypes, outs, propertyName))
  }

}

object ProcessInstance {
  def signalsToJson(signals: MutableList[(SignalInstance, String)]): MutableList[String] = {
    signals.map{ case (signal, suffix) => signal.name + suffix }
  }
}
