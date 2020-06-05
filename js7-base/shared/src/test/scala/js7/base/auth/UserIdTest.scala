package js7.base.auth

import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class UserIdTest extends AnyFreeSpec {

  "Invalid UserId" in {
    assert(UserId.checked("").isLeft)
    assert(UserId.checked("a?").isLeft)
    assert(UserId.checked("a?b").isLeft)
    assert(UserId.checked("a/b").isLeft)
    assert(UserId.checked("/root").isLeft)
    assert(UserId.checked(".").isLeft)
    assert(UserId.checked("..").isLeft)
    assert(UserId.checked(".hidden").isLeft)
    assert(UserId.checked("Master--100").isLeft)  // "--" is used for history journal files
  }

  "Valid UserId" in {
    UserId("a")
    UserId("å")
    UserId("テスト")
    UserId("0")
    UserId("ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÅÜabcdefghijklmnopqrstuvwxyzäöåüß0123456789.-_")
    UserId("a.")
    UserId("A-B")
  }
}
