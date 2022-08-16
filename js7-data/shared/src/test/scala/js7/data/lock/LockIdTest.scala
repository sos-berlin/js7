package js7.data.lock

import js7.base.standards.Js7PathValidatorTest
import js7.base.test.OurTestSuite

final class LockPathTest extends OurTestSuite
{
  "Invalid path" in {
    Js7PathValidatorTest.checkInvalid("LockPath", LockPath.checked)
  }

  "Valid paths" in {
    Js7PathValidatorTest.checkValid(LockPath.checked)
  }
}
