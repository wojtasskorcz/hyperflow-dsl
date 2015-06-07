package hdsl.parser.structures

sealed trait Conjunction

case object Arrow extends Conjunction // "->"

case object JoinArrow extends Conjunction // "-|>"

case class PartialJoinArrow(num: Int) extends Conjunction // "-|2>"

case class BlockingPartialJoinArrow(num: Int) extends Conjunction // "-||2>
