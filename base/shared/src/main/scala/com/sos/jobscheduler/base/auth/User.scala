package com.sos.jobscheduler.base.auth

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
      Left(UserDoesNotHavePermissionProblem(id, requiredPermission))
    else
      Checked.unit

  final def hasPermissions(requiredPermissions: Set[Permission]): Boolean =
    requiredPermissions forall hasPermission

  final def hasPermission(requiredPermission: Permission): Boolean =
    grantedPermissions.contains(requiredPermission) || grantedPermissions.contains(SuperPermission)

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
