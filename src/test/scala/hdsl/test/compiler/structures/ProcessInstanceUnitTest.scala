package hdsl.test.compiler.structures

import hdsl.compiler.structures.ProcessInstance
import hdsl.parser.structures.rhs.Atomic
import hdsl.parser.structures.wfelems.ProcessClass
import hdsl.test.UnitSpec
import org.junit.Assert._

import scala.collection.mutable

class ProcessInstanceUnitTest extends UnitSpec {

  test("That setting properties works properly") {
    val processClass = ProcessClass("testClass", null, null, null)
    processClass.setProperty(List("lvl1", "end2"), Atomic(2))
    processClass.setProperty(List("lvl1", "lvl2", "end3"), Atomic(3))
    // these should be overwritten by the instance settings
    processClass.setProperty(List("lvl1", "lvl2", "end3.1"), Atomic(999))
    processClass.setProperty(List("lvl1", "end2.1", "fakeEnd"), Atomic(999))
    val instance = ProcessInstance("testInstance", processClass, null)
    instance.setProperty(List("end1"), Atomic(1))
    instance.setProperty(List("lvl1", "end2.1"), Atomic(4))
    instance.setProperty(List("lvl1", "lvl2", "end3.1"), Atomic(5))
    instance.setProperty(List("lvl1", "lvl2.1", "end3.2"), Atomic(6))

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
