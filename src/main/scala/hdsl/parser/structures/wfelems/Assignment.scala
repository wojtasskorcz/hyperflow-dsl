package hdsl.parser.structures.wfelems

import hdsl.parser.structures.DotNotationAccessor

case class Assignment(lhs: List[DotNotationAccessor], rhs: Any) extends AnyRef with WfElem {

}
