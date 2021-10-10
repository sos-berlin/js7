package js7.base.utils

import js7.base.utils.Tests.isTest
import org.scalatest.freespec.AnyFreeSpec

final class TestsTest extends AnyFreeSpec
{
  "isTest" in {
    assert(isTest)
  }
}
