package js7.base.auth

import js7.base.generic.SecretString
import org.scalatest.freespec.AnyFreeSpec

final class SessionTokenTest extends AnyFreeSpec
{
  "toString" in {
    assert(SessionToken(SecretString("123,SECRET")).toString == "Session:123")

    assert(SessionToken(SecretString("SECRET")).toString == "Session:?")
    assert(SessionToken(SecretString(",SECRET")).toString == "Session:?")
    assert(SessionToken(SecretString("x,SECRET")).toString == "Session:?")
    assert(SessionToken(SecretString("1x,SECRET")).toString == "Session:?")
  }
}
