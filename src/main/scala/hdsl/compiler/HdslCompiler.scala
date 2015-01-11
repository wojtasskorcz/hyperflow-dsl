package hdsl.compiler

import hdsl.MutableMap
import hdsl.compiler.structures.SignalInstance
import hdsl.parser.structures.DotNotationAccessor
import hdsl.parser.structures.rhs.SignalInstantiation
import hdsl.parser.structures.wfelems.{Assignment, SignalClass, WfElem, ProcessClass}
import hdsl.implicits.MyImplicits._

import scala.collection.mutable

object HdslCompiler {

  def compile(wfElems: List[WfElem]): Map[String, Any] = {
    prepareDataStructures(wfElems)
    var wf = Map[String, Any]()
//    val signalDeclarations = wfElems.collect({case elem: Signal => elem.name -> elem}).toMap
//    val signalInstances = compileSignals(wfElems)
//    wf += "signals" -> compileSignals(wfElems)
    wf
  }

  def prepareDataStructures(wfElems: List[WfElem]) = {
    val signalClasses = mutable.Map.empty[String, SignalClass]
    val processClasses = mutable.Map.empty[String, ProcessClass]
    val signalInstances = mutable.Map.empty[String, SignalInstance]

    wfElems.foreach({
      case signalClass: SignalClass => signalClasses.putUnique(signalClass.name -> signalClass)
      case processClass: ProcessClass => processClasses.putUnique(processClass.name -> processClass)
      case Assignment(lhs, rhs: SignalInstantiation) => signalInstances += prepareExplicitSignalInstance(lhs, rhs, signalClasses)
      case _ => "unimplemented"
    })
  }

  def prepareExplicitSignalInstance(lhs: List[DotNotationAccessor], instantiation: SignalInstantiation,
                            signalClasses: MutableMap[String, SignalClass]): (String, SignalInstance) = {
    val signalInstanceName = lhs match {
      case List(DotNotationAccessor(List(name: String))) => name
      case x => throw new RuntimeException(s"A signal instance cannot be assigned to $x")
    }
    val signalClass = signalClasses.get(instantiation.className) match {
      case Some(signalClass: SignalClass) => signalClass
      case None => throw new RuntimeException(
        s"Cannot instantiate signal $signalInstanceName. Signal class ${instantiation.className} not found")
    }
    (signalInstanceName, SignalInstance(signalInstanceName, signalClass, instantiation))
  }

}
