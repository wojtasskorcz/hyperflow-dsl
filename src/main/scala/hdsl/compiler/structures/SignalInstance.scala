package hdsl.compiler.structures

import hdsl.parser.structures.Arg
import hdsl.parser.structures.rhs.{Atomic, SignalInstantiation}
import hdsl.parser.structures.wfelems.SignalClass

case class SignalInstance(name: String, signalClass: SignalClass, instantiation: SignalInstantiation) {

  checkArgsCompatibility()

  def checkArgsCompatibility() = {
    signalClass.args.zipAll(instantiation.args, null, null).foreach({
      case (null, _) => throw new RuntimeException(
        s"Cannot instantiate signal $name. Too many arguments for class ${signalClass.name}")
      case (_, null) => throw new RuntimeException(
        s"Cannot instantiate signal $name. Too little arguments for class ${signalClass.name}")
      case (Arg(_, "String", _), Atomic(s: String)) => "OK"
      case (Arg(argName, argType, _), Atomic(value: Any)) => throw new RuntimeException(
        s"Cannot instantiate signal $name. $value cannot be passed to argument $argName of type $argType")
    })
  }

}
