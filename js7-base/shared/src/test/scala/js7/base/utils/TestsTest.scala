package js7.base.utils

import js7.base.test.OurTestSuite
import js7.base.utils.Tests.isTest

final class TestsTest extends OurTestSuite:
  "isTest" in:
    assert(isTest)
