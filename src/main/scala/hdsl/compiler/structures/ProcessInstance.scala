package hdsl.compiler.structures

import hdsl._
import hdsl.parser.structures.Arg
import hdsl.parser.structures.rhs.{Expr, ProcessInstantiation}
import hdsl.parser.structures.traits.{Instantiated, PropertyContainer}
import hdsl.parser.structures.wfelems.{SignalClass, ProcessClass}

import scala.collection.mutable

/**
 * `name` is the name of the instance that will be set in generated JSON (may be sth like `anonymous$12` or `pArr$0[3]`
 * it is NOT the name of the identifier (variable) this process instance is assigned to
 */
case class ProcessInstance(name: String, instantiation: ProcessInstantiation) extends PropertyContainer with Instantiated {

  final val processClass = Wf.processClasses.get(instantiation.className) match {
    case Some(processClass: ProcessClass) => processClass
    case None => throw new RuntimeException(
      s"Cannot instantiate process $name. Process class ${instantiation.className} not found")
  }

  addAllProperties(processClass)

  val ins = mutable.MutableList.empty[String]
  val outs = mutable.MutableList.empty[String]
  val sticky = mutable.MutableList.empty[String]
  var choiceSource: Option[ProcessInstance] = None
  var processType = "dataflow"
  var partialJoinNum: Option[Int] = None

  override def putInstanceOnlyToVisible(visibleName: String): Unit =  Wf.visibleProcessInstances += visibleName -> this

  override def putInstanceToVisibleAndAll(visibleName: String): Unit =  Wf.putProcessInstance(visibleName -> this)

  def toMap: MutableMap[String, Any] = {
    val outMap = mutable.Map[String, Any]("name" -> name, "function" -> processClass.invocation.name)
    outMap ++= resolvedPropertiesMap()
    outMap += "type" -> processType
    outMap += "ins" -> ins
    outMap += "outs" -> outs
    outMap += "sticky" -> sticky
    outMap
  }

  def addInput(signal: SignalInstance, suffix: String = "") = {
    if (ins.size == processClass.args.size) {
      throw new RuntimeException(s"Cannot add signal ${signal.name} as input to process ${name}. Too many inputs.")
    }
    val inputArg = processClass.args(ins.size)
    if (signal.signalClass.name != inputArg.argType) {
      throw new RuntimeException(s"Input number ${ins.size} of process $name is of type ${inputArg.argType} cannot be set to signal ${signal.name} of class ${signal.signalClass.name}")
    }
    if (signal.choiceSource.nonEmpty && processType != "join") {
      choiceSource match {
        case None => processClass.returnTypes.size match {
          case 1 => choiceSource = signal.choiceSource
          case n => throw new RuntimeException(s"Cannot set signal ${signal.name} deriving from a choice process as input to process $name as the process doesn't have exactly one output")
        }
        case Some(alreadySetChoiceSource) => throw new RuntimeException(s"Cannot set two signals deriving from a choice process as inputs to the same process $name. Conflicting signal: ${signal.name}")
      }
    }
    if (inputArg.modifiers.contains("sticky")) {
      sticky += signal.name
    }
    ins += signal.name + suffix
  }

  def addOutput(signal: SignalInstance) = {
    if (outs.size >= processClass.returnTypes.size) {
      throw new RuntimeException(s"Cannot add another output signal ${signal.name} to process $name")
    }
    if (choiceSource.nonEmpty && processType != "join") {
      signal.choiceSource = choiceSource
    }
    outs += signal.name
  }

  def getNextOutSignalClass: Option[SignalClass] =
    if (outs.size >= processClass.returnTypes.size) None
    else Some(Wf.signalClasses(processClass.returnTypes(outs.size)))

}
