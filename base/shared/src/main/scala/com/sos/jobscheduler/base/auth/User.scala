package com.sos.jobscheduler.base.auth

import cats.data.Validated.Invalid
import com.sos.jobscheduler.base.problem.{Checked, Problem}

/**
  * User for a user.
  *
  * @author Joacim Zschimmer
  */
trait User
{
  def id: UserId
  def hashedPassword: HashedPassword
  def grantedPermissions: Set[Permission]

  final def checkPermission(requiredPermission: Permission): Checked[Unit] =
    if (!hasPermission(requiredPermission))
      Invalid(Problem(s"User does not have the required permission '${requiredPermission.name}'"))
    else
      Checked.unit

  final def hasPermissions(requiredPermissions: Set[Permission]): Boolean =
    requiredPermissions forall grantedPermissions.contains

  final def hasPermission(requiredPermission: Permission): Boolean =
    grantedPermissions contains requiredPermission

  final def isAnonymous = id.isAnonymous
}

object User
{
  trait Companion[U <: User] {
    def addPermissions(user: U, permissions: Set[Permission]): U
  }
}
