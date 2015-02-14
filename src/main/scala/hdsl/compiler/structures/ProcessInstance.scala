package hdsl.compiler.structures

import hdsl._
import hdsl.parser.structures.rhs.{Expr, ProcessInstantiation}
import hdsl.parser.structures.traits.PropertyContainer
import hdsl.parser.structures.wfelems.{SignalClass, ProcessClass}

import scala.collection.mutable

case class ProcessInstance(name: String, processClass: ProcessClass, instantiation: ProcessInstantiation) extends PropertyContainer {

  addAllProperties(processClass)

  val ins = mutable.MutableList.empty[String]
  val outs = mutable.MutableList.empty[String]

  def toMap: MutableMap[String, Any] = {
    val outMap = mutable.Map[String, Any]("name" -> name, "function" -> processClass.invocation.name)
    outMap ++= resolvedPropertiesMap()
    outMap += "ins" -> ins
    outMap += "outs" -> outs
    outMap
  }

  def addInput(signal: SignalInstance, suffix: String = "") = {
    if (ins.size == processClass.args.size) {
      throw new RuntimeException(s"Cannot add signal ${signal.name} as input to process ${name}. Too many inputs.")
    }
    if (signal.signalClass.name != processClass.args(ins.size).argType) {
      throw new RuntimeException(s"Input number ${ins.size} of process $name is of type ${processClass.args(ins.size).argType} cannot be set to signal ${signal.name} of class ${signal.signalClass.name}")
    }
    ins += signal.name + suffix
  }

  def addOutput(signal: SignalInstance) = {
    if (outs.nonEmpty) {
      throw new RuntimeException(s"Cannot add another output signal ${signal.name} to process $name")
    }
    if (processClass.returnType == "Unit") {
      throw new RuntimeException(s"Cannot add output signal ${signal.name} to process $name that returns 'Unit'")
    }
    outs += signal.name
  }

  def getOutSignalClass: SignalClass = Wf.signalClasses(processClass.returnType)

}
