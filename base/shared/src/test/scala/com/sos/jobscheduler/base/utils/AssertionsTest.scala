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

    val file = implicitly[sourcecode.File].value
    assertThat(file endsWith "/base/shared/src/test/scala/com/sos/jobscheduler/base/utils/AssertionsTest.scala")

    assert(intercept[AssertionError] { assertThat(1 == 0) }.getMessage
      == s"assertThat(==) failed in com.sos.jobscheduler.base.utils.AssertionsTest, $file:17")  // Only "==" is shown

    val a = 1
    assert(intercept[AssertionError] { assertThat(a == 0) }.getMessage
      == s"assertThat(a == 0) failed in com.sos.jobscheduler.base.utils.AssertionsTest, $file:21")
  }
}
