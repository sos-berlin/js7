package js7.base.auth

import js7.base.generic.SecretString
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SimpleUserTest extends AnyFreeSpec
{
  "Anonymous" in {
    assert(SimpleUser.TestAnonymous.hashedPassword equalsClearText SecretString(""))
    assert(SimpleUser(UserId.Anonymous, HashedPassword.newEmpty()).grantedPermissions.isEmpty)

    intercept[RuntimeException] {
      SimpleUser(UserId.Anonymous, HashedPassword.newEmpty(), Set(ValidUserPermission))
    }
  }

  "Not Anonymous" in {
    SimpleUser(UserId.Anonymous, HashedPassword.newEmpty()).grantedPermissions == Set(ValidUserPermission)
  }
}
