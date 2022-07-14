package js7.base.auth

import js7.base.auth.User.UserDoesNotHavePermissionProblem
import js7.base.auth.UserTest.*
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class UserTest extends AnyFreeSpec
{
  "checkPermissions" in {
    assert(testUser(Set.empty).checkPermissions() == Right(()))
    assert(testUser(Set(A)).checkPermissions() == Right(()))
    assert(testUser(Set(A)).checkPermissions(A) == Right(()))
    assert(testUser(Set(A, B)).checkPermissions(A) == Right(()))

    val noPermissionUser = testUser(Set.empty)
    assert(noPermissionUser.checkPermissions(A) == Left(UserDoesNotHavePermissionProblem(noPermissionUser.id, A)))

    val aUser = testUser(Set(A))
    assert(aUser.checkPermissions(A) == Right(()))
    assert(aUser.checkPermissions(A, B) == Left(UserDoesNotHavePermissionProblem(aUser.id, B)))

    val bUser = testUser(Set(B))
    assert(bUser.checkPermissions(A) == Left(UserDoesNotHavePermissionProblem(bUser.id, A)))
    assert(bUser.checkPermissions(A, B) == Left(UserDoesNotHavePermissionProblem(bUser.id, A)))
  }

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
