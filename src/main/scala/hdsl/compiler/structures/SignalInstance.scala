package hdsl.compiler.structures

import hdsl.MutableMap
import hdsl.parser.structures.Arg
import hdsl.parser.structures.rhs.{ExprList, Expr, SignalInstantiation}
import hdsl.parser.structures.traits.{PropertyContainer, Instantiated}

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

  /**
   * If this signal derives from a choice process, we reference this process instance here, so subsequent signals in
   * the workflow may inherit this reference and know their original choice-source
   */
  var choiceSource: Option[ProcessInstance] = None

  checkArgsCompatibility()

  private def checkArgsCompatibility() = {
    signalClass.args.zipAll(instantiation.args, null, null).foreach({
      case (null, _) => throw new RuntimeException(s"Cannot instantiate signal $name. Too many arguments for class ${signalClass.name}")
      case (_, null) => throw new RuntimeException(s"Cannot instantiate signal $name. Too few arguments for class ${signalClass.name}")
      case (Arg(_, "String", _), expr: Expr) => if (expr.evaluate.isInstanceOf[String]) "OK" else throw new RuntimeException(s"Cannot instantiate signal $name. Expression $expr does not evaluate to the require type 'String'")
      case (Arg(_, "Array", _), _: ExprList) => "OK"
      case (Arg(argName, argType, _), Expr(value: Any)) => throw new RuntimeException(s"Cannot instantiate signal $name. $value cannot be passed to argument $argName of type $argType")
    })
  }

  override def putInstanceOnlyToVisible(visibleName: String): Unit =  Wf.visibleSignalInstances += visibleName -> this

  override def putInstanceToVisibleAndAll(visibleName: String): Unit =  Wf.putSignalInstance(visibleName -> this)

  def toMap: MutableMap[String, Any] = {

    def argumentsMap: MutableMap[String, Any] = {
      val immutableMap: Map[String, Any] = signalClass.args.zip(instantiation.args).map({
        case (arg, value: Expr) => arg.name -> value.evaluate
        case (arg, value: ExprList) => arg.name -> value.parts.map(expr => expr.evaluate)
      }).toMap
      collection.mutable.Map(immutableMap.toSeq: _*)
    }

    val outMap = mutable.Map[String, Any]("name" -> name)
    if (signalClass.control.nonEmpty) outMap += "control" -> signalClass.control.get
    // if signal class declared as ClassName(:argType), assign directly to 'data' property, otherwise create map within 'data'
    if (argumentsMap.nonEmpty) outMap += "data" -> (if (signalClass.args(0).name == null) argumentsMap(null) else List(argumentsMap))
    outMap
  }

}
