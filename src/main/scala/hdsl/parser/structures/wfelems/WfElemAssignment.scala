package hdsl.parser.structures.wfelems

import hdsl.parser.structures.DotNotationAccessor
import hdsl.parser.structures.rhs.Rhs

case class WfElemAssignment(lhs: DotNotationAccessor, rhs: Rhs) extends Assignment {

}
