package hdsl.compiler

import hdsl.compiler.structures.SignalInstance
import hdsl.parser.structures.{Arg, DotNotationAccessor}
import hdsl.parser.structures.rhs.{Atomic, SignalInstantiation}
import hdsl.parser.structures.wfelems.{Assignment, SignalClass, WfElem}

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
    val signalClasses: Map[String, SignalClass] = wfElems.collect({case elem: SignalClass => elem.name -> elem}).toMap
    var signalInstances = prepareExplicitSignalInstances(wfElems, signalClasses)
    println (signalInstances)
  }

  def prepareExplicitSignalInstances(wfElems: List[WfElem], signalClasses: Map[String, SignalClass]): Map[String, SignalInstance] = {
    val signals = wfElems.collect({
      case Assignment(lhs, rhs: SignalInstantiation)  => prepareExplicitSignalInstance(lhs, rhs, signalClasses)
    }).toMap
    signals
  }

  def prepareExplicitSignalInstance(lhs: List[DotNotationAccessor], instantiation: SignalInstantiation,
                            signalClasses: Map[String, SignalClass]): (String, SignalInstance) = {
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
