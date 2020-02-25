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
    case class TestUser(id: UserId, hashedPassword: HashedPassword, grantedPermissions: Set[Permission]) extends User

    def testUser(grantedPermissions: Set[Permission]) =
      TestUser(UserId("someone"), HashedPassword.newEmpty(), grantedPermissions)

    assert(testUser(Set.empty) hasPermissions Set.empty)
    assert(testUser(Set(A)) hasPermissions Set.empty)
    assert(testUser(Set(A)) hasPermissions Set(A))
    assert(testUser(Set(A, B)) hasPermissions Set(A))

    assert(!testUser(Set.empty).hasPermissions(Set(A)))
    assert(!testUser(Set(B)).hasPermissions(Set(A)))
    assert(!testUser(Set(B)).hasPermissions(Set(A, B)))
  }
}
