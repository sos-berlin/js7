package js7.data.lock

import js7.base.standards.Js7PathValidatorTest
import org.scalatest.freespec.AnyFreeSpec

final class LockIdTest extends AnyFreeSpec
{
  "Invalid path" in {
    Js7PathValidatorTest.checkInvalid("LockId", LockId.checked)
  }

  "Valid paths" in {
    Js7PathValidatorTest.checkValid(LockId.checked)
  }
}
