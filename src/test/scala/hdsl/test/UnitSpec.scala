package hdsl.test

import hdsl.compiler.structures.Wf
import org.scalatest.mock.MockitoSugar
import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
 * Always mix in BeforeAndAfterEach as the last trait!
 */
abstract class UnitSpec extends FunSuite with MockitoSugar with BeforeAndAfterEach {

  override def beforeEach: Unit = {
    Wf.init()
  }

}
