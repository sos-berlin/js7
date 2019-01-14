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
  if (id == UserId.Anonymous && grantedPermissions.contains(ValidUserPermission))
    throw new IllegalArgumentException("Anonymous must not have ValidUserPermission")
}

object SimpleUser extends User.Companion[SimpleUser] {
  /** The unauthenticated, anonymous user without permissions.. */
  val Anonymous = SimpleUser(UserId.Anonymous, HashedPassword.newEmpty, grantedPermissions = PermissionBundle.empty)
  val System = SimpleUser(UserId("System"), HashedPassword.MatchesNothing, PermissionBundle(Set(ChangeRepoPermission)))
  implicit val companion = this

  def addPermissions(user: SimpleUser, permissionBundle: PermissionBundle): SimpleUser =
    user.copy(grantedPermissions = user.grantedPermissions ++ permissionBundle)

  def apply(
    id: UserId,
    hashedPassword: HashedPassword = HashedPassword.MatchesNothing,
    grantedPermissions: PermissionBundle = PermissionBundle.empty)
  = new SimpleUser(
      id,
      hashedPassword,
      PermissionBundle(grantedPermissions.permissions ++
        (if (id != UserId.Anonymous) Set(ValidUserPermission) else Set.empty)))
}
