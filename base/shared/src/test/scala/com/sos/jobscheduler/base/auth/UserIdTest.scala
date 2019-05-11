package com.sos.jobscheduler.base.auth

import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class UserIdTest extends FreeSpec {

  "Invalid UserId" in {
    assert(UserId.checked("").isInvalid)
    assert(UserId.checked("a?").isInvalid)
    assert(UserId.checked("a?b").isInvalid)
    assert(UserId.checked("a/b").isInvalid)
    assert(UserId.checked("/root").isInvalid)
    assert(UserId.checked(".").isInvalid)
    assert(UserId.checked("..").isInvalid)
    assert(UserId.checked(".hidden").isInvalid)
    assert(UserId.checked("Master--100").isInvalid)  // "--" is used for history journal files
  }

  "Valid UserId" in {
    UserId("a")
    UserId("å")
    UserId("テスト")
    UserId("0")
    UserId("ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÅÜabcdefghijklmnopqrstuvwxyzäöåüß0123456789.-_")
    UserId("a.")
  }
}
