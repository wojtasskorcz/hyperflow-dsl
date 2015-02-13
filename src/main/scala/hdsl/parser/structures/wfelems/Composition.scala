package hdsl.parser.structures.wfelems

import hdsl.compiler.structures.{Wf, SignalInstance, ProcessInstance}
import hdsl.parser.structures.{DotNotationAccessor, CompositionElem}
import hdsl.parser.structures.rhs.{ProcessInstantiation, SignalInstantiation}

import scala.collection.mutable

case class Composition(elems: List[CompositionElem]) extends WfElem {

  private val tmpProcesses = mutable.Map.empty[String, ProcessInstance]

  def compose() = {
    // for each pair of adjacent composition elements
    elems zip elems.tail foreach {
      case (from, to) if from.isSignalElem() || to.isProcessElem(tmpProcesses) => setInputs(to, from)
      case (from, to) if from.isProcessElem(tmpProcesses) || to.isSignalElem() => setOutputs(from, to)
      case _ => throw new RuntimeException("TODO")
    }
  }

  private def setInputs(processElem: CompositionElem, signalElem: CompositionElem) = {
    val processInstance = processElem match {
      case CompositionElem(List(processName), null) => Wf.visibleProcessInstances.get(processName) match {
        case Some(instance) => instance
        case None => Wf.processClasses.get(processName) match {
          case Some(processClass) => createAnonymousProcess(processClass)
          case None => throw new RuntimeException(s"Could not find process instance nor process class named $processName")
        }
      }
    }

    signalElem match {
      case CompositionElem(signalNames, null) => signalNames.foreach(
        signalName => processInstance.addInput(Wf.visibleSignalInstances(signalName))
      )
      case CompositionElem(List(signalName), DotNotationAccessor(List(countSourceSignalName, "count"))) => {
        val countSignal = createCountSignal(countSourceSignalName)
        processInstance.addInput(Wf.visibleSignalInstances(signalName), s":${countSignal.name}")
      }

    }
  }

  private def setOutputs(processElem: CompositionElem, signalElem: CompositionElem): Unit = {
    val processInstance = processElem match {
      case CompositionElem(List(processName), null) => Wf.visibleProcessInstances.get(processName) match {
        case Some(instance) => instance
        case None => tmpProcesses.get(processName) match {
          case Some(instance) => instance
          case None => throw new RuntimeException(s"Could not find process $processName to set its output")
        }
      }
    }
    
    signalElem match {
      case CompositionElem(signalNames, _) => signalNames.foreach {
        case signalName if Wf.visibleSignalInstances.contains(signalName) => throw new RuntimeException("TODO")
        case signalName => processInstance.addOutput(createOutputSignal(signalName, processInstance))
      }
    }
  }

  private def createAnonymousProcess(processClass: ProcessClass): ProcessInstance = {
    val processInstance = ProcessInstance(Wf.getNextAnonymousName, processClass, ProcessInstantiation(processClass.name))
    tmpProcesses += processClass.name -> processInstance
    Wf.allProcessInstances += processInstance
    processInstance
  }

  private def createAnonymousSignal(signalClass: SignalClass): SignalInstance = {
    if (signalClass.args != Nil) {
      throw new RuntimeException(s"Could not create an anonymous singal of class ${signalClass.name}, as the signal takes arguments")
    }
    val signalInstance = SignalInstance(Wf.getNextAnonymousName, signalClass, SignalInstantiation(signalClass.name, Nil))
    Wf.allSignalInstances += signalInstance
    signalInstance
  }

  private def createOutputSignal(signalName: String, processInstance: ProcessInstance): SignalInstance = {
    val signalClass = processInstance.getOutSignalClass
    if (signalClass.args.nonEmpty) {
      throw new RuntimeException(s"Cannot automatically generate signal $signalName of class ${signalClass.name} because the class takes arguments")
    }
    val signalInstance = SignalInstance(signalName, signalClass, SignalInstantiation(signalClass.name, Nil))
    Wf.putSignalInstance(signalName -> signalInstance)
    signalInstance
  }

  private def createCountSignal(countSourceSignalName: String): SignalInstance = {
    val countSourceProcesses = Wf.allProcessInstances.filter(processInstance => processInstance.outs.contains(countSourceSignalName))
    if (countSourceProcesses.length != 1) {
      throw new RuntimeException(s"Could not uniquely find a process, which could be the source of count signal $countSourceSignalName. Number of processes found: ${countSourceProcesses.length}")
    }
    val countSourceProcess = countSourceProcesses(0)
    val signalInstance = createAnonymousSignal(Wf.signalClasses("Signal"))
    countSourceProcess.outs.transform(
      outSignal => if(outSignal == countSourceSignalName) s"$outSignal:${signalInstance.name}" else outSignal
    )
    signalInstance
  }

}
