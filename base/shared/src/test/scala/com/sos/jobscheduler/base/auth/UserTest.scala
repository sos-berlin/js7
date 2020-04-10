package com.sos.jobscheduler.base.auth

import com.sos.jobscheduler.base.auth.UserTest._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class UserTest extends AnyFreeSpec
{
  "hasPermissions" in {
    assert(testUser(Set.empty) hasPermissions Set.empty)
    assert(testUser(Set(A)) hasPermissions Set.empty)
    assert(testUser(Set(A)) hasPermissions Set(A))
    assert(testUser(Set(A, B)) hasPermissions Set(A))

    assert(!testUser(Set.empty).hasPermissions(Set(A)))
    assert(!testUser(Set(B)).hasPermissions(Set(A)))
    assert(!testUser(Set(B)).hasPermissions(Set(A, B)))
  }

  "SuperPermission" in {
    assert(testUser(Set(SuperPermission)) hasPermissions Set.empty)
    assert(testUser(Set(SuperPermission)) hasPermissions Set(A))
    assert(testUser(Set(SuperPermission, A)) hasPermissions Set.empty)
    assert(testUser(Set(SuperPermission, A)) hasPermissions Set(A))
    assert(testUser(Set(SuperPermission, A)) hasPermissions Set(B))
  }
}

private object UserTest
{
  private case object A extends Permission
  private case object B extends Permission

  private case class TestUser(id: UserId, hashedPassword: HashedPassword, grantedPermissions: Set[Permission]) extends User

  private def testUser(grantedPermissions: Set[Permission]) =
    TestUser(UserId("someone"), HashedPassword.newEmpty(), grantedPermissions)
}
