package com.sos.jobscheduler.common.utils

import com.sos.jobscheduler.common.utils.Tests.isTest
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TestsTest extends AnyFreeSpec
{
  "isTest" in {
    assert(isTest)
  }
}
