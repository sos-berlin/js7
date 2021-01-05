package js7.base.auth

import js7.base.generic.SecretString
import org.scalatest.freespec.AnyFreeSpec

final class SessionTokenTest extends AnyFreeSpec
{
  "toString" in {
    assert(SessionToken(SecretString("123,SECRET")).toString == "▶123")

    assert(SessionToken(SecretString("SECRET")).toString == "SessionToken")
    assert(SessionToken(SecretString(",SECRET")).toString == "SessionToken")
    assert(SessionToken(SecretString("x,SECRET")).toString == "SessionToken")
    assert(SessionToken(SecretString("1x,SECRET")).toString == "SessionToken")
  }
}
