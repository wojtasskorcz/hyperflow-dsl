package hdsl.parser.structures.wfelems

import hdsl.parser.structures.Arg

case class Signal(name: String, args: List[Arg]) extends AnyRef with WfElem {

  // Signals can't have arguments with modifiers
  require(args.forall(arg => arg.modifiers == Nil))

  // Signals must have an explicit type on all arguments
  require(args.forall(arg => arg.argType != null))

}
