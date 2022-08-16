package js7.base.utils

import js7.base.test.OurTestSuite
import org.scalatest.matchers.should.Matchers.*

final class HasCloserTest extends OurTestSuite {

  "onClose" in {
    class A extends HasCloser {
      var closed = false
      onClose { closed = true }
    }
    val a = new A
    a.closed shouldBe false
    a.close()
    a.closed shouldBe true
  }
}
