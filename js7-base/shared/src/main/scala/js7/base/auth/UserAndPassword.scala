package js7.base.auth

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.generic.SecretString
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class UserAndPassword(userId: UserId, password: SecretString):

  override def toString = s"UserAndPassword($userId)"


object UserAndPassword:
  implicit def apply(userAndPassword: (UserId, SecretString)): UserAndPassword =
    new UserAndPassword(userAndPassword._1, userAndPassword._2)

  val jsonCodec: Codec.AsObject[UserAndPassword] =
    given Codec[SecretString] = SecretString.jsonCodec
    deriveCodec[UserAndPassword]
