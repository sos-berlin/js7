package com.sos.scheduler.engine.common.auth

import com.google.common.hash.HashCode
import com.google.common.hash.Hashing.sha512
import com.sos.scheduler.engine.common.auth.ConfigPasswordValidator._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.typesafe.config.{Config, ConfigException}
import java.nio.charset.StandardCharsets.UTF_8
import scala.util.control.NonFatal

/**
  * Uses a `Config` with key/value pairs to validates a user password pair.
  * The configured passwords are prefixed with a hash scheme or `"plain:"`.
  * <pre>
  * tobbe = "plain:PASSWORD"
  * lisbeth = "sha512:130c7809c9e5a8d81347b55f5c82c3a7407f4b41b461eb641887d276b11af4b575c5a32d1cf104e531c700e4b1ddd75b27b9e849576f6dfb8ca42789fbc7ece2"
  * </pre>
  *
  * @author Joacim Zschimmer
  */
final class ConfigPasswordValidator(config: Config) extends (UserAndPassword ⇒ Boolean) {

  def apply(userAndPassword: UserAndPassword) =
    hashedPasswordOption(userAndPassword.user) exists validatePassword(userAndPassword)

  private[auth] def hashedPasswordOption(user: String): Option[String] =
    try Some(config.getString(user))
    catch {
      case _: ConfigException.Missing ⇒
        logger.warn(s"Unknown user '$user'")
        None
      case NonFatal(t) ⇒
        logger.warn(s"Invalid configuration entry for user '$user': $t", t)
        None
    }

  private[auth] def validatePassword(userAndPassword: UserAndPassword)(hashedPassword: String): Boolean = {
    import userAndPassword.{password, user}
    hashedPassword match {
      case EntryRegex("plain", pw) ⇒ password.string == pw
      case EntryRegex("sha512", pw) ⇒ sha512.hashString(password.string, UTF_8) == HashCode.fromString(pw)
      case EntryRegex(_, _) ⇒
        logger.error(s"Unknown password encoding scheme for user '$user'")
        false
      case o ⇒
        logger.error(s"Missing password encoding scheme for user '$user'. Try to prefix the configured password with plain: or sha512:")
        false
    }
  }
}

private object ConfigPasswordValidator {
  private val EntryRegex = "([^:]+):(.*)".r
  private val logger = Logger(getClass)
}
