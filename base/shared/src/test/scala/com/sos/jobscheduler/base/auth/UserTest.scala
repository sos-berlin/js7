package com.sos.jobscheduler.base.auth

import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class UserTest extends FreeSpec
{
  "hasPermissions" in {
    case object A extends Permission
    case object B extends Permission
    case class TestUser(id: UserId, hashedPassword: HashedPassword, grantedPermissions: PermissionBundle) extends User

    def testUser(grantedPermissions: PermissionBundle) =
      TestUser(UserId("someone"), HashedPassword.newEmpty, grantedPermissions)

    assert(testUser(PermissionBundle.empty) hasPermissions PermissionBundle(Set.empty))
    assert(testUser(PermissionBundle(Set(A))) hasPermissions PermissionBundle(Set.empty))
    assert(testUser(PermissionBundle(Set(A))) hasPermissions PermissionBundle(Set(A)))
    assert(testUser(PermissionBundle(Set(A, B))) hasPermissions PermissionBundle(Set(A)))

    assert(!testUser(PermissionBundle.empty).hasPermissions(PermissionBundle(Set(A))))
    assert(!testUser(PermissionBundle(Set(B))).hasPermissions(PermissionBundle(Set(A))))
    assert(!testUser(PermissionBundle(Set(B))).hasPermissions(PermissionBundle(Set(A, B))))
  }
}
