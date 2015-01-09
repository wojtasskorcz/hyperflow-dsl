package hdsl.parser.structures

case class Process(name: String, args: List[Arg], returnType: String, settings: List[Assignment],
                   invocation: FunctionInvocation) {

}
