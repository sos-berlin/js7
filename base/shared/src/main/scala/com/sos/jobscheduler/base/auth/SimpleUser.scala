package com.sos.jobscheduler.base.auth

import com.sos.jobscheduler.base.utils.ScalaUtils._
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
    throw new IllegalArgumentException("Anonymous must not have ValidUserPermission")
  // SuperPermission is allowed for empowered Anonymous (public = on | loopback-is-public = on)
}

object SimpleUser extends User.Companion[SimpleUser]
{
  /** The unauthenticated, anonymous user without permissions, for testing. */
  val TestAnonymous = SimpleUser(UserId.Anonymous, HashedPassword.newEmpty(), grantedPermissions = Set.empty)
  val System = SimpleUser(UserId("System"), HashedPassword.MatchesNothing, Set(SuperPermission))
  implicit val companion = this

  def addPermissions(user: SimpleUser, permissions: Set[Permission]): SimpleUser =
    reuseIfEqual(user, user.copy(
      grantedPermissions = user.grantedPermissions ++ permissions))

  def apply(
    id: UserId,
    hashedPassword: HashedPassword = HashedPassword.MatchesNothing,
    grantedPermissions: Set[Permission] = Set.empty)
  = new SimpleUser(
      id,
      hashedPassword,
      grantedPermissions ++ (!id.isAnonymous thenSet ValidUserPermission))
}
