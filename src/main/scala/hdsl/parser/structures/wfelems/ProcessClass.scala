package hdsl.parser.structures.wfelems

import hdsl.parser.structures.traits.PropertyContainer
import hdsl.parser.structures.{Arg, FunctionInvocation}

/**
 * @param returnTypes contains signal class names of the out signals of this process class. If the return type was
 *                    specified as "Unit", the list will be empty.
 */
case class ProcessClass(name: String, args: List[Arg], returnTypes: List[String], invocation: FunctionInvocation)
  extends WfElem with PropertyContainer {

}
