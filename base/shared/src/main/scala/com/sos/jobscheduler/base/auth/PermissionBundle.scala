package com.sos.jobscheduler.base.auth

import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class PermissionBundle(permissions: Set[Permission])
{
  def isEmpty = permissions.isEmpty

  def ++(o: PermissionBundle) = PermissionBundle(permissions ++ o.permissions)

  def -(o: Permission) = PermissionBundle(permissions - o)

  def contains(requiredPermissions: PermissionBundle): Boolean =
    requiredPermissions.permissions forall contains

  def contains(requiredPermission: Permission): Boolean =
    permissions contains requiredPermission
}

object PermissionBundle {
  val empty = new PermissionBundle(Set.empty)

  implicit def fromSet(permissions: Set[Permission]): PermissionBundle = new PermissionBundle(permissions)

  implicit def of(permission: Permission): PermissionBundle = new PermissionBundle(Set(permission))

  def of(permissions: Permission*) =
    new PermissionBundle(permissions.toSet)
}
