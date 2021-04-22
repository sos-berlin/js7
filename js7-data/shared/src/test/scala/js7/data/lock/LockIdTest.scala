package js7.data.lock

import js7.base.standards.Js7PathValidatorTest
import org.scalatest.freespec.AnyFreeSpec

final class LockPathTest extends AnyFreeSpec
{
  "Invalid path" in {
    Js7PathValidatorTest.checkInvalid("LockPath", LockPath.checked)
  }

  "Valid paths" in {
    Js7PathValidatorTest.checkValid(LockPath.checked)
  }
}
