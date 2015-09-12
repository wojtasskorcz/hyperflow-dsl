package hdsl.parser.structures.wfelems

import hdsl.parser.structures.rhs.{ExprList, Expr, Rhs}
import hdsl.parser.structures.traits.PropertyContainer
import hdsl.parser.structures.Arg

/**
 * @param returnTypes contains signal class names of the out signals of this process class. If the return type was
 *                    specified as "Unit", the list will be empty.
 */
case class ProcessClass(name: String, args: List[Arg], returnTypes: List[String], function: String)
  extends WfElem with PropertyContainer {

  /**
   * Override the method from PropertyContainer. Normally, the rhs is instantly evaluated, but in case of classes this
   * method is called from the parser and the evaluation must be defered.
   */
  override def setProperty(path: List[String], rhs: Rhs) = flatProperties += path -> rhs
  
  def evaluateProperties(): Unit = {
    flatProperties.transform { case (key, value) => value match {
      case expr: Expr => expr.evaluate
      case exprList: ExprList => exprList.parts.map(expr => expr.evaluate)
      case otherValue: Any => throw new RuntimeException("Only Expr or List[Expr] should be stored as properties")
    }}
  }

}
