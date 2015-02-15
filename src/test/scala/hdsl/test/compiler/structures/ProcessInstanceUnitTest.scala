package hdsl.test.compiler.structures

import hdsl.compiler.structures.{Wf, ProcessInstance}
import hdsl.parser.structures.rhs.{ProcessInstantiation, Expr}
import hdsl.parser.structures.wfelems.ProcessClass
import hdsl.test.UnitSpec
import org.junit.Assert._

import scala.collection.mutable

class ProcessInstanceUnitTest extends UnitSpec {

  test("That setting properties works properly") {
    val processClass = ProcessClass("testClass", null, null, null)
    Wf.putProcessClass("testClass", processClass)
    processClass.setProperty(List("lvl1", "end2"), Expr(2))
    processClass.setProperty(List("lvl1", "lvl2", "end3"), Expr(3))
    // these should be overwritten by the instance settings
    processClass.setProperty(List("lvl1", "lvl2", "end3.1"), Expr(999))
    processClass.setProperty(List("lvl1", "end2.1", "fakeEnd"), Expr(999))
    val instance = ProcessInstance("testInstance", ProcessInstantiation("testClass", null))
    instance.setProperty(List("end1"), Expr(1))
    instance.setProperty(List("lvl1", "end2.1"), Expr(4))
    instance.setProperty(List("lvl1", "lvl2", "end3.1"), Expr(5))
    instance.setProperty(List("lvl1", "lvl2.1", "end3.2"), Expr(6))

    val finalProperties = mutable.Map(
      "end1" -> 1,
      "lvl1" -> mutable.Map(
        "end2" -> 2,
        "end2.1" -> 4,
        "lvl2" -> mutable.Map(
          "end3" -> 3,
          "end3.1" -> 5),
        "lvl2.1" -> mutable.Map(
          "end3.2" -> 6)))

    assertEquals(finalProperties, instance.resolvedPropertiesMap())
  }

}
