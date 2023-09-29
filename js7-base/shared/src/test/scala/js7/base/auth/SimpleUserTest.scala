package js7.base.auth

import js7.base.generic.SecretString
import js7.base.test.OurTestSuite

/**
  * @author Joacim Zschimmer
  */
final class SimpleUserTest extends OurTestSuite:
  "Anonymous" in:
    assert(SimpleUser.TestAnonymous.hashedPassword equalsClearText SecretString(""))
    assert(SimpleUser(UserId.Anonymous, HashedPassword.newEmpty()).grantedPermissions.isEmpty)

    intercept[RuntimeException]:
      SimpleUser(UserId.Anonymous, HashedPassword.newEmpty(), Set(ValidUserPermission))

  "Not Anonymous" in:
    SimpleUser(UserId.Anonymous, HashedPassword.newEmpty()).grantedPermissions == Set(ValidUserPermission)
