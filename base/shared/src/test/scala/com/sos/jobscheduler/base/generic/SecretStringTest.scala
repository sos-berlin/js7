package com.sos.jobscheduler.base.generic

import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SecretStringTest extends FreeSpec {

  "equals" in {
    assert(SecretString("") == SecretString(""))
    assert(SecretString("abc") != SecretString(""))
    assert(SecretString("") != SecretString("abc"))
    assert(SecretString("abc") == SecretString("abc"))
    assert(SecretString("abc.") != SecretString("abc"))
    assert(SecretString("abc") != SecretString("abc."))
    assert(SecretString("abc") != SecretString("ab."))
  }

  "toString does not disclose the secret" in {
    val secret = SecretString("TEST-SECRET")
    assert(secret.toString == "SecretString")
    assert(s"$secret" == "SecretString")
  }
}
