package simple_prelude

import org.junit.Assert._
import org.scalatest.FunSuite
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

class PreludeSpec extends FunSuite {
  test("something") {
    assertTrue(true)
  }
}
