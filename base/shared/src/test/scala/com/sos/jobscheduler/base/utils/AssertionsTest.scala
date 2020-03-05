package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.utils.Assertions._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AssertionsTest extends FreeSpec
{
  "assertThat" in {
    assertThat(true)

    val a = 9
    assert(intercept[IllegalStateException] { assertThat(0 == a) }.getMessage
      == s"assertThat(0 == a) failed in com.sos.jobscheduler.base.utils.AssertionsTest, AssertionsTest.scala:15")

    assert(intercept[IllegalStateException] { assertThat(a == 0, s"a=$a") }.getMessage
      == s"assertThat(a == 0) failed in com.sos.jobscheduler.base.utils.AssertionsTest, AssertionsTest.scala:18, a=9")
  }
}
