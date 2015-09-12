package hdsl.parser.structures.wfelems

import hdsl.compiler.HdslCompiler
import hdsl.compiler.structures.{ProcessInstance, SignalInstance, Wf}
import hdsl.parser.structures.rhs.{Expr, ProcessInstantiation, SignalInstantiation}
import hdsl.parser.structures._

import scala.collection.mutable

case class Composition(elems: List[CompositionElem], conjs: List[Conjunction]) extends WfElem {

  private val tmpProcesses = mutable.Map.empty[String, ProcessInstance]

  def compose() = {

    def setJoinProcess(processInstance: ProcessInstance) = {
      processInstance.processType = "join"
    }

    def createSetPartialJoinProcess(num: Int): ProcessInstance => Unit = {
      (processInstance: ProcessInstance) => {
        processInstance.processType = "join"
        processInstance.joinCount = Some(num)
      }
    }

    def createSetBlockingPartialJoinProcess(num: Int): ProcessInstance => Unit = {
      (processInstance: ProcessInstance) => {
        processInstance.processType = "join"
        processInstance.joinCount = Some(num)
        processInstance.isBlockingJoin = true
      }
    }

    // for each pair of adjacent composition elements and their conjunction operator
    (elems, elems.tail, conjs).zipped.toList foreach {
      case (from, to, Arrow) if from.isSignalElem() || to.isProcessElem(tmpProcesses) => setInputs(to, from)
      case (from, to, Arrow) if from.isProcessElem(tmpProcesses) || to.isSignalElem() => setOutputs(from, to, false)
      case (from, to, JoinArrow) if from.isSignalElem() || to.isProcessElem(tmpProcesses) => setInputs(to, from, setJoinProcess)
      case (from, to, JoinArrow) if from.isProcessElem(tmpProcesses) || to.isSignalElem() => setOutputs(from, to, true)
      case (from, to, PartialJoinArrow(num)) if from.isSignalElem() || to.isProcessElem(tmpProcesses) =>
        setInputs(to, from, createSetPartialJoinProcess(num))
      case (from, to, BlockingPartialJoinArrow(num)) if from.isSignalElem() || to.isProcessElem(tmpProcesses) =>
        setInputs(to, from, createSetBlockingPartialJoinProcess(num))
      case x => throw new RuntimeException("TODO " + x)
    }
  }

  private def setInputs(processElem: CompositionElem, signalElem: CompositionElem,
                        modifyProcess: ProcessInstance => Unit = _ => Unit) = {
    val processInstance = processElem match {
      case CompositionElem(List(accessor: DotNotationAccessor), null) =>
        Wf.visibleProcessInstances.get(accessor.stringifiedBase) match {
          case Some(instance) => instance
          case None => Wf.processClasses.get(accessor.stringifiedBase) match {
            case Some(processClass) => createAnonymousProcess(processClass)
            case None => throw new RuntimeException(s"Could not find process instance nor process class named ${accessor.stringifiedBase}")
          }
        }
    }

    modifyProcess(processInstance)

    signalElem match {
      case CompositionElem(List(signalName), Left(accessor@DotNotationAccessor(parts))) if parts.last == "count" => {
        val countSignal = createCountSignal(accessor.stringifiedBase)
        processInstance.addInput(Wf.visibleSignalInstances(signalName.stringifiedBase), s":${countSignal.name}")
      }
      case CompositionElem(List(signalName), Right(num)) => {
        processInstance.addInput(Wf.visibleSignalInstances(signalName.stringifiedBase), s":$num")
      }
      case CompositionElem(signalNames, null) => signalNames.foreach(signalName => {
        val signalInstance = Wf.visibleSignalInstances.getOrElse(signalName.stringifiedBase,
          createSignal(signalName, processInstance.getNextInSignalClass))
        processInstance.addInput(signalInstance)
      })
    }
  }

  private def createAnonymousProcess(processClass: ProcessClass): ProcessInstance = {
    val processInstance = ProcessInstance(Wf.getNextAnonymousName, ProcessInstantiation(processClass
      .name, null))
    tmpProcesses += processClass.name -> processInstance
    Wf.allProcessInstances += processInstance
    processInstance
  }

  private def createCountSignal(countSourceSignalName: String): SignalInstance = {
    val countSourceProcesses = Wf.allProcessInstances.filter(processInstance => processInstance.outs.exists {
      case (signal, suffix) => signal.name == countSourceSignalName
    })
    if (countSourceProcesses.length != 1) {
      throw new RuntimeException(s"Could not uniquely find a process, which could be the source of count signal $countSourceSignalName. Number of processes found: ${countSourceProcesses.length}")
    }
    val countSourceProcess = countSourceProcesses(0)
    val signalInstance = createAnonymousSignal(Wf.signalClasses(HdslCompiler.countSignalClassName))
    countSourceProcess.outs.transform {
      case (signal, _) => if (signal.name == countSourceSignalName) (signal, ":" + signalInstance.name) else (signal, "")
    }
    signalInstance
  }

  private def createAnonymousSignal(signalClass: SignalClass): SignalInstance = {
    if (signalClass.args != Nil) {
      throw new RuntimeException(s"Could not create an anonymous singal of class ${signalClass.name}, as the signal takes arguments")
    }
    val signalInstance = SignalInstance(Wf.getNextAnonymousName, SignalInstantiation(signalClass.name,
      Nil, null))
    Wf.allSignalInstances += signalInstance
    signalInstance
  }

  private def setOutputs(processElem: CompositionElem, signalElem: CompositionElem, markChoiceSource: Boolean): Unit = {
    val processInstance = processElem match {
      case CompositionElem(List(DotNotationAccessor(List(processName: String))), null) => Wf.visibleProcessInstances.get(processName) match {
        case Some(instance) => instance
        case None => tmpProcesses.get(processName) match {
          case Some(instance) => instance
          case None => Wf.processClasses.get(processName) match {
            case Some(processClass) => createAnonymousProcess(processClass)
            case None => throw new RuntimeException(s"Could not find process instance nor process class named $processName")
          }
        }
      }
    }

    if (markChoiceSource) {
      processInstance.processType = "choice"
    }

    signalElem match {
      case CompositionElem(signalNames, _) => signalNames.foreach(signalName => {
        val signalInstance = Wf.visibleSignalInstances.getOrElse(signalName.stringifiedBase,
          createSignal(signalName, processInstance.getNextOutSignalClass))
        processInstance.addOutput(signalInstance)
        if (markChoiceSource) {
          signalInstance.choiceSource = Some(processInstance)
        }
      })
    }
  }

  /**
   * signalName may be pure name (e.g. 'someSig') or an array accessor (e.g. 'sigArr[idx]'). In the latter case,
   * appropriate array is created (along with all its individual signals).
   */
  private def createSignal(signalName: DotNotationAccessor, optSignalClass: Option[SignalClass]): SignalInstance = {
    require(signalName == signalName.base)
    val signalClass = optSignalClass.getOrElse(throw new RuntimeException(s"Cannot create signal ($signalName)"))
    if (signalClass.args.nonEmpty) {
      throw new RuntimeException(s"Cannot automatically generate signal $signalName of class ${signalClass.name} because the class takes arguments")
    }
    val loopIdxVar = Wf.variables.getOrElse("$loopIdxVar", null)
    val arraySize = signalName.parts.size match {
      case 1 => null
      case 2 => signalName.parts(1).asInstanceOf[Expr].value match {
        case `loopIdxVar` if loopIdxVar != null => Expr(Wf.variables("$arraySize"))
        case x => throw new RuntimeException(s"Cannot automatically generate array of signals ${signalName} because its index ${signalName.parts(1)} is different from the loop index ${Wf.variables("$loopIdxVar")}")
      }
    }
    val signalInstantiation = SignalInstantiation(signalClass.name, Nil, arraySize)
    signalInstantiation.instantiate(DotNotationAccessor(List(signalName.parts(0))))
    Wf.visibleSignalInstances(signalName.stringifiedBase)
  }

}
