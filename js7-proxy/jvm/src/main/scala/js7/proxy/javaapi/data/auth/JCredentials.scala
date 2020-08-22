package js7.proxy.javaapi.data.auth

import js7.base.annotation.javaApi
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.proxy.javaapi.data.auth.JCredentials._

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
