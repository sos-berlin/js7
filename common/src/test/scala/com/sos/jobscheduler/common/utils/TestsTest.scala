package com.sos.jobscheduler.common.utils

import com.sos.jobscheduler.common.utils.Tests.isTest
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TestsTest extends FreeSpec
{
  "isTest" in {
    assert(isTest)
  }
}
