package com.sos.jobscheduler.base.auth

import com.sos.jobscheduler.base.utils.ScalazStyle._

/**
  * @author Joacim Zschimmer
  */
final case class SimpleUser private(
  id: UserId,
  hashedPassword: HashedPassword,
  grantedPermissions: Set[Permission])
extends User
{
  if (id.isAnonymous && grantedPermissions.contains(ValidUserPermission))
    throw new IllegalArgumentException("Anonymous cannot not have ValidUserPermission")
}

object SimpleUser extends User.Companion[SimpleUser] {
  /** The unauthenticated, anonymous user without permissions.. */
  val Anonymous = SimpleUser(UserId.Anonymous, HashedPassword.newEmpty, grantedPermissions = Set.empty)
  val System = SimpleUser(UserId("System"), HashedPassword.MatchesNothing, Set(UpdateRepoPermission))
  implicit val companion = this

  def addPermissions(user: SimpleUser, permissions: Set[Permission]): SimpleUser =
    user.copy(grantedPermissions = user.grantedPermissions ++ permissions)

  def apply(
    id: UserId,
    hashedPassword: HashedPassword = HashedPassword.MatchesNothing,
    grantedPermissions: Set[Permission] = Set.empty)
  = new SimpleUser(
      id,
      hashedPassword,
      grantedPermissions ++ (!id.isAnonymous thenSet ValidUserPermission))
}
