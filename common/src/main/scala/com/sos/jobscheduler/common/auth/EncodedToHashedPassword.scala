package com.sos.jobscheduler.common.auth

import com.google.common.hash.Hashing.sha512
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.utils.ScalaUtils.implicits._
import com.sos.jobscheduler.common.auth.EncodedToHashedPassword._
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.scalautil.Logger
import com.typesafe.config.Config
import java.nio.charset.StandardCharsets.UTF_8

/**
  * Extracts the hashed password and the hashing algorithm from an encoded password.
  * The encoded passwords are prefixed with a hash scheme or `"plain:"`.
  * <pre>
  * tobbe = "plain:PASSWORD"
  * lisbeth = "sha512:130c7809c9e5a8d81347b55f5c82c3a7407f4b41b461eb641887d276b11af4b575c5a32d1cf104e531c700e4b1ddd75b27b9e849576f6dfb8ca42789fbc7ece2"
  * </pre>
  * The SHA512 check sum can be generated with: sha512sum <code><(echo -n "password")</code>.
  *
  * @author Joacim Zschimmer
  */
final class EncodedToHashedPassword(userToEncodedPassword: UserId ⇒ Option[SecretString])
extends (UserId ⇒ Option[HashedPassword]){

  def apply(userId: UserId) =
    userToEncodedPassword(userId) flatMap { o ⇒ toHashedPassword(userId, o) }
}

object EncodedToHashedPassword {
  private val logger = Logger(getClass)
  private val EntryRegex = "([^:]+):(.*)".r
  private[auth] val sha512Hasher = { o: String ⇒ sha512.hashString(o: String, UTF_8).toString } withToString "sha512"
  private[auth] val identityHasher = { o: String ⇒ identity(o) } withToString "identity"

  def fromSubConfig(config: Config): EncodedToHashedPassword =
    new EncodedToHashedPassword(userId ⇒ config.optionAs[SecretString](userId.string))

  private def toHashedPassword(userId: UserId, encodedPassword: SecretString) =
    encodedPassword.string match {
      case EntryRegex("plain", pw) ⇒
        Some(HashedPassword(SecretString(pw), identityHasher))

      case EntryRegex("sha512", pw) ⇒
        Some(HashedPassword(SecretString(pw), sha512Hasher))

      case EntryRegex(_, _) ⇒
        logger.error(s"Unknown password encoding scheme for user '$userId'")
        None

      case _ ⇒
        logger.error(s"Missing password encoding scheme for user '$userId'. Try to prefix the configured password with 'plain:' or 'sha512:'")
        None
    }

}
