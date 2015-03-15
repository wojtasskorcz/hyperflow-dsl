package hdsl.parser.structures.wfelems

import hdsl.parser.structures.Arg

case class SignalClass(name: String, args: List[Arg]) extends AnyRef with WfElem {

  // SignalClasses can't have arguments with modifiers
  require(args.forall(arg => arg.modifiers == Nil))

  // SignalClasses must have an explicit type on all arguments
  require(args.forall(arg => arg.argType != null))

  var control: Option[String] = None

}
