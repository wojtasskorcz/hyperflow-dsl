package hdsl.parser.structures.wfelems

import hdsl.parser.structures.traits.PropertyContainer
import hdsl.parser.structures.Arg

/**
 * @param returnTypes contains signal class names of the out signals of this process class. If the return type was
 *                    specified as "Unit", the list will be empty.
 */
case class ProcessClass(name: String, args: List[Arg], returnTypes: List[String], function: String)
  extends WfElem with PropertyContainer {

}
