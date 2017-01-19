package com.sos.scheduler.engine.base.generic

import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SecretStringTest extends FreeSpec {

  "toString does not disclose the secret" in {
    val secret = SecretString("TEST-SECRET")
    assert(secret.toString == "SecretString")
    assert(s"$secret" == "SecretString")
  }
}
