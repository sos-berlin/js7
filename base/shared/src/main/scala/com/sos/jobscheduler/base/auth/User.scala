package com.sos.jobscheduler.base.auth

import cats.data.Validated.Invalid
import com.sos.jobscheduler.base.auth.User._
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
      Invalid(UserDoesNotHavePermissionProblem(id, requiredPermission))
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

  final case class UserDoesNotHavePermissionProblem(userId: UserId, permission: Permission) extends Problem.Coded {
    def arguments = Map(
      "userId" -> userId.string,
      "permission" -> permission.name
    )
  }
}
