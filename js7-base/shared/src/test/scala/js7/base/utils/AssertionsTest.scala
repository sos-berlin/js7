package js7.base.utils

import js7.base.utils.Assertions.*
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AssertionsTest extends AnyFreeSpec
{
  "assertThat" in {
    assertThat(true)

    val a = 9
    assert(intercept[AssertionError] { assertThat(0 == a) }.getMessage
      == s"assertThat(0 == a) failed in js7.base.utils.AssertionsTest, AssertionsTest.scala:15")

    assert(intercept[AssertionError] { assertThat(a == 0, s"a=$a") }.getMessage
      == s"assertThat(a == 0) failed in js7.base.utils.AssertionsTest, AssertionsTest.scala:18, a=9")
  }
}
