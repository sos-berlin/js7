package com.sos.jobscheduler.base.auth

/**
  * @author Joacim Zschimmer
  */
final case class SimpleUser private(
  id: UserId,
  hashedPassword: HashedPassword,
  grantedPermissions: PermissionBundle)
extends User
{
  if (id == UserId.Anonymous && grantedPermissions.contains(KnownUserPermission))
    throw new IllegalArgumentException("Anonymous must not have KnownUserPermission")
}

object SimpleUser {
  /** The unauthenticated, anonymous user without permissions.. */
  val Anonymous = SimpleUser(UserId.Anonymous, HashedPassword.newEmpty, grantedPermissions = PermissionBundle.empty)
  val System = SimpleUser(UserId("System"), HashedPassword.MatchesNothing)

  def apply(
    id: UserId,
    hashedPassword: HashedPassword,
    grantedPermissions: PermissionBundle = PermissionBundle.empty)
  = new SimpleUser(
      id,
      hashedPassword,
      PermissionBundle(grantedPermissions.permissions ++
        (if (id != UserId.Anonymous) Set(KnownUserPermission) else Set.empty)))
}
