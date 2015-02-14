package hdsl.parser.structures.wfelems

import hdsl.parser.structures.rhs.Expr

case class VarAssignment(varName: String, rhs: Expr) extends Assignment {

}
