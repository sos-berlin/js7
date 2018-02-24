package com.sos.jobscheduler.base.auth

import com.sos.jobscheduler.base.generic.SecretString
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class UserAndPassword(userId: UserId, password: SecretString)

object UserAndPassword {
  implicit def apply(userAndPassword: (UserId, SecretString)): UserAndPassword =
    new UserAndPassword(userAndPassword._1, userAndPassword._2) }
