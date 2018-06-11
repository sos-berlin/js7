package com.sos.jobscheduler.common.auth

import com.google.common.base.Splitter
import com.google.common.hash.Hashing.sha512
import com.sos.jobscheduler.base.auth.{HashedPassword, Permission, PermissionBundle, User, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.utils.ScalaUtils.implicits._
import com.sos.jobscheduler.common.auth.IdToUser._
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.scalautil.Logger
import com.typesafe.config.Config
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

/**
  * Extracts the hashed password and the hashing algorithm from an encoded password.
  * The encoded passwords are prefixed with a hash scheme or `"plain:"`.
  * <pre>
  * tobbe = "plain:PASSWORD"
  * lisbeth = "sha512:130c7809c9e5a8d81347b55f5c82c3a7407f4b41b461eb641887d276b11af4b575c5a32d1cf104e531c700e4b1ddd75b27b9e849576f6dfb8ca42789fbc7ece2"
  * </pre>
  * How to generate SHA512?
  * <ul>
  *   <li>Gnu (Linux): <code>sha512sum <(echo -n "password")</code>.
  *   <li>MacOS: <code>shasum -a 512 <(echo -n "password")</code>.
  * </ul>
  *
  * @author Joacim Zschimmer
  */
final class IdToUser[U <: User](
  userIdToRaw: UserId ⇒ Option[RawUserAccount],
  toUser: (UserId, HashedPassword, PermissionBundle) ⇒ U,
  toPermission: PartialFunction[String, Permission])
extends (UserId ⇒ Option[U]) {

  def apply(userId: UserId) =
    userIdToRaw(userId).flatMap(o ⇒ rawToUser(userId, o))

  private def rawToUser(userId: UserId, raw: RawUserAccount): Option[U] =
    for (hashedPassword ← toHashedPassword(userId, raw.encodedPassword))
      yield toUser(userId, hashedPassword.hashAgainRandom, PermissionBundle((raw.permissions map toPermission.lift).flatten))
}

object IdToUser {
  private val logger = Logger(getClass)
  private val EntryRegex = "([^:]+):(.*)".r
  private val PermissionSplitter = Splitter.on("""[ \t]+""".r.pattern)

  def fromConfig[U <: User](
    config: Config,
    toUser: (UserId, HashedPassword, PermissionBundle) ⇒ U,
    toPermission: PartialFunction[String, Permission] = PartialFunction.empty)
  : IdToUser[U] = {
    val cfg = config.getConfig("jobscheduler.auth.users")
    def userIdToRaw(userId: UserId) =
      if (cfg.hasPath(userId.string))
        Try(cfg.getConfig(userId.string)) match {
          case Success(c) ⇒
            for {
              encodedPassword ← c.optionAs[SecretString]("password")
              permissions = c.optionAs[String]("permissions") map PermissionSplitter.split map (_.asScala.toSet) getOrElse Set.empty
            } yield RawUserAccount(encodedPassword = encodedPassword, permissions = permissions)

          case Failure(_: com.typesafe.config.ConfigException.WrongType) ⇒
            cfg.optionAs[SecretString](userId.string) map (o ⇒ RawUserAccount(encodedPassword = o, permissions = Set.empty))

          case Failure(t) ⇒
            throw t
        }
      else
        None
    new IdToUser(userIdToRaw, toUser, toPermission)
  }

  private val sha512Hasher = { o: String ⇒ sha512.hashString(o: String, UTF_8).toString } withToString "sha512"
  private val identityHasher = { o: String ⇒ identity(o) } withToString "identity"

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
