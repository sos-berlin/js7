package com.sos.jobscheduler.base.auth

/**
  * User for a user.
  *
  * @author Joacim Zschimmer
  */
trait User {
  def id: UserId
  def hashedPassword: HashedPassword
  def grantedPermissions: PermissionBundle

  final def hasPermissions(requiredPermissions: PermissionBundle): Boolean =
    grantedPermissions contains requiredPermissions

  final def hasPermission(requiredPermission: Permission): Boolean =
    grantedPermissions contains requiredPermission
}
