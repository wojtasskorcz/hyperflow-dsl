package hdsl.compiler.structures

import hdsl.MutableMap
import hdsl.parser.structures.Arg
import hdsl.parser.structures.rhs.{Atomic, SignalInstantiation}
import hdsl.parser.structures.wfelems.SignalClass

import scala.collection.mutable

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

  private def argumentsMap: MutableMap[String, Any] = {
    val immutableMap: Map[String, Any] = signalClass.args.zip(instantiation.args).map({
      case (arg, value) => arg.name -> value.evaluate
    }).toMap
    collection.mutable.Map(immutableMap.toSeq: _*)
  }

  def toMap: MutableMap[String, Any] = {
    val outMap = mutable.Map[String, Any]("name" -> name)
    outMap += "data" -> List(argumentsMap)
    outMap
  }

}
