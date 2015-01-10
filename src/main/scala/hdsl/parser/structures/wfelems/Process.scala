package hdsl.parser.structures.wfelems

import hdsl.parser.structures.{Arg, FunctionInvocation}

case class Process(name: String, args: List[Arg], returnType: String, settings: List[Assignment],
                   invocation: FunctionInvocation) extends AnyRef with WfElem {

}
