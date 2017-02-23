package com.sos.scheduler.engine.common.auth

import com.google.common.hash.HashCode
import com.google.common.hash.Hashing.sha512
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.auth.EncodedPasswordValidator._
import com.sos.scheduler.engine.common.configutils.Configs.ConvertibleConfig
import com.sos.scheduler.engine.common.scalautil.Logger
import com.typesafe.config.Config
import java.nio.charset.StandardCharsets.UTF_8
import scala.util.{Failure, Success, Try}

/**
  * Validates an `UserAndPassword` agains an encoded password.
  * The encoded passwords are prefixed with a hash scheme or `"plain:"`.
  * <pre>
  * tobbe = "plain:PASSWORD"
  * lisbeth = "sha512:130c7809c9e5a8d81347b55f5c82c3a7407f4b41b461eb641887d276b11af4b575c5a32d1cf104e531c700e4b1ddd75b27b9e849576f6dfb8ca42789fbc7ece2"
  * </pre>
  * The SHA512 check sum can be generated with: sha512sum <code><(echo -n "password")</code>.
  *
  * @author Joacim Zschimmer
  */
final class EncodedPasswordValidator(userToEncodedPassword: UserId ⇒ Option[SecretString])
extends (UserAndPassword ⇒ Boolean) {

  def apply(userAndPassword: UserAndPassword) =
    hashedPasswordOption(userAndPassword.userId) exists validatePassword(userAndPassword)

  private[auth] def hashedPasswordOption(user: UserId): Option[SecretString] =
    Try { userToEncodedPassword(user) }
    match {
      case Success(o) ⇒
        if (o.isEmpty) logger.warn(s"Unknown user '$user'")
        o
      case Failure(t) ⇒
        logger.error(s"Invalid configuration entry for user '$user': $t", t)
        None
    }

  private[auth] def validatePassword(userAndPassword: UserAndPassword)(hashedPassword: SecretString): Boolean = {
    import userAndPassword.{password, userId}
    hashedPassword.string match {
      case EntryRegex("plain", pw) ⇒ pw == password.string
      case EntryRegex("sha512", pw) ⇒ sha512.hashString(password.string, UTF_8) == HashCode.fromString(pw)
      case EntryRegex(_, _) ⇒
        logger.error(s"Unknown password encoding scheme for user '$userId'")
        false
      case o ⇒
        logger.error(s"Missing password encoding scheme for user '$userId'. Try to prefix the configured password with 'plain:' or 'sha512:'")
        false
    }
  }
}

object EncodedPasswordValidator {
  private val EntryRegex = "([^:]+):(.*)".r
  private val logger = Logger(getClass)

  def fromSubConfig(config: Config): EncodedPasswordValidator =
    new EncodedPasswordValidator(userId ⇒ config.optionAs[SecretString](userId.string))
}
