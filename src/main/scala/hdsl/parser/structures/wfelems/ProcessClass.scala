package hdsl.parser.structures.wfelems

import hdsl.parser.structures.traits.PropertyContainer
import hdsl.parser.structures.{Arg, FunctionInvocation}

case class ProcessClass(name: String, args: List[Arg], returnType: String, invocation: FunctionInvocation)
  extends WfElem with PropertyContainer {

}
