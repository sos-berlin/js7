package com.sos.scheduler.engine.base.generic

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class SecretStringTest extends FreeSpec {

  "toString does not disclose the secret" in {
    val secret = SecretString("TEST-SECRET")
    assert(secret.toString == "SecretString")
    assert(s"$secret" == "SecretString")
  }
}
