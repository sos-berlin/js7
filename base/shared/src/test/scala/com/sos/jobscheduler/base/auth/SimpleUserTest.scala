package com.sos.jobscheduler.base.auth

import com.sos.jobscheduler.base.generic.SecretString
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SimpleUserTest extends FreeSpec
{
  "Anonymous" in {
    assert(SimpleUser.Anonymous.hashedPassword equalsClearText SecretString(""))
    assert(SimpleUser(UserId.Anonymous, HashedPassword.newEmpty).grantedPermissions == PermissionBundle.empty)

    intercept[RuntimeException] {
      SimpleUser(UserId.Anonymous, HashedPassword.newEmpty, PermissionBundle(Set(ValidUserPermission)))
    }
  }

  "Not Anonymous" in {
    SimpleUser(UserId.Anonymous, HashedPassword.newEmpty).grantedPermissions == PermissionBundle(Set(ValidUserPermission))
  }
}
