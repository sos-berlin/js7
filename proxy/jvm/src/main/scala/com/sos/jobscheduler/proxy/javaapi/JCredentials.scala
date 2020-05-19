package com.sos.jobscheduler.proxy.javaapi

import com.sos.jobscheduler.base.annotation.javaApi
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.proxy.javaapi.JCredentials._

@javaApi
sealed trait JCredentials
{
  def toUnderlying: Option[UserAndPassword] =
    this match {
      case NoCredentials => None
      case o: JUserAndPassword => Some(o.underlying)
    }
}

@javaApi
object JCredentials
{
  final val noCredentials = NoCredentials

  object NoCredentials extends JCredentials

  def of(userId: String, password: String): JCredentials =
    JUserAndPassword(UserAndPassword(UserId(userId), SecretString(password)))

  final case class JUserAndPassword(underlying: UserAndPassword)
  extends JCredentials
}
