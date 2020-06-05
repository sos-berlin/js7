package js7.common.utils

import js7.common.utils.Tests.isTest
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
