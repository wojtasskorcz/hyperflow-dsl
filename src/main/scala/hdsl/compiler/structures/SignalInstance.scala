package hdsl.compiler.structures

import hdsl.MutableMap
import hdsl.parser.structures.Arg
import hdsl.parser.structures.rhs.{Expr, SignalInstantiation}
import hdsl.parser.structures.traits.Instantiated

import scala.collection.mutable

/**
 * `name` is the name of the instance that will be set in generated JSON (may be sth like `anonymous$12` or `sigArr$0[3]`
 * it is NOT the name of the identifier (variable) this signal instance is assigned to
 */
case class SignalInstance(name: String, instantiation: SignalInstantiation) extends Instantiated {

  final val signalClass = Wf.signalClasses.get(instantiation.className) match {
    case Some(signalClass) => signalClass
    case None => throw new RuntimeException(s"Cannot instantiate signal $name. Signal class ${instantiation.className} not found")
  }

  checkArgsCompatibility()

  private def checkArgsCompatibility() = {
    signalClass.args.zipAll(instantiation.args, null, null).foreach({
      case (null, _) => throw new RuntimeException(
        s"Cannot instantiate signal $name. Too many arguments for class ${signalClass.name}")
      case (_, null) => throw new RuntimeException(
        s"Cannot instantiate signal $name. Too few arguments for class ${signalClass.name}")
      case (Arg(_, "String", _), Expr(s: String)) => "OK"
      case (Arg(argName, argType, _), Expr(value: Any)) => throw new RuntimeException(
        s"Cannot instantiate signal $name. $value cannot be passed to argument $argName of type $argType")
    })
  }

  override def putInstanceOnlyToVisible(visibleName: String): Unit =  Wf.visibleSignalInstances += visibleName -> this

  override def putInstanceToVisibleAndAll(visibleName: String): Unit =  Wf.putSignalInstance(visibleName -> this)

  def toMap: MutableMap[String, Any] = {

    def argumentsMap: MutableMap[String, Any] = {
      val immutableMap: Map[String, Any] = signalClass.args.zip(instantiation.args).map({
        case (arg, value) => arg.name -> value.evaluate
      }).toMap
      collection.mutable.Map(immutableMap.toSeq: _*)
    }

    val outMap = mutable.Map[String, Any]("name" -> name)
    outMap += "data" -> List(argumentsMap)
    outMap
  }

}
