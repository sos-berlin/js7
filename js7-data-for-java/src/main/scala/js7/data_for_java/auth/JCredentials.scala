package js7.data_for_java.auth

import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.data_for_java.auth.JCredentials.*

@javaApi
sealed trait JCredentials:
  def toScala: Option[UserAndPassword] =
    this match
      case NoCredentials => None
      case o: JUserAndPassword => Some(o.asScala)

@javaApi
object JCredentials:
  final val noCredentials = NoCredentials

  object NoCredentials extends JCredentials

  @Nonnull
  def of(@Nonnull userId: String, @Nonnull password: String): JCredentials =
    JUserAndPassword(UserAndPassword(UserId(userId), SecretString(password)))

  final case class JUserAndPassword(asScala: UserAndPassword)
  extends JCredentials
