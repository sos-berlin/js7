package com.sos.jobscheduler.common.auth

import com.google.common.hash.Hashing.sha512
import com.sos.jobscheduler.base.auth.{HashedPassword, Permission, User, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.utils.ScalaUtils.implicits._
import com.sos.jobscheduler.base.utils.ScalazStyle._
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
  userIdToRaw: UserId => Option[RawUserAccount],
  toUser: (UserId, HashedPassword, Set[Permission]) => U,
  toPermission: PartialFunction[String, Permission])
extends (UserId => Option[U]) {

  private lazy val someAnonymous = Some(toUser(UserId.Anonymous, HashedPassword.newEmpty, Set.empty))

  def apply(userId: UserId) =
    if (userId.isAnonymous)
      someAnonymous
    else
      userIdToRaw(userId).flatMap(o => rawToUser(userId, o))

  private def rawToUser(userId: UserId, raw: RawUserAccount): Option[U] =
    for (hashedPassword <- toHashedPassword(userId, raw.encodedPassword))
      yield toUser(userId, hashedPassword.hashAgainRandom, raw.permissions.map(toPermission.lift).flatten)
}

object IdToUser
{
  private val logger = Logger(getClass)
  private val EntryRegex = "([^:]+):(.*)".r
  private val UsersConfigPath = "jobscheduler.auth.users"

  def fromConfig[U <: User](
    config: Config,
    toUser: (UserId, HashedPassword, Set[Permission]) => U,
    toPermission: PartialFunction[String, Permission] = PartialFunction.empty)
  : IdToUser[U] = {
    val cfg = config.getConfig(UsersConfigPath)

    def userIdToRaw(userId: UserId): Option[RawUserAccount] =
      if (cfg.hasPath(userId.string))
        existentUserIdToRaw(userId)
      else {
        logger.debug(s"""Configuration files ("private.conf") does not have an entry '$UsersConfigPath.${userId.string}'""")
        None
      }

    def existentUserIdToRaw(userId: UserId) =
      Try(cfg.getConfig(userId.string)) match {
        case Failure(_: com.typesafe.config.ConfigException.WrongType) =>  // Entry is not a configuration object {...} but a string (the password)
          cfg.optionAs[SecretString](userId.string) map (o =>
            RawUserAccount(encodedPassword = o, permissions = Set.empty))

        case Failure(t) =>
          throw t

        case Success(c) =>
          for {
            encodedPassword <- c.optionAs[SecretString]("password")
            permissions = c.hasPath("permissions").thenList(c.getStringList("permissions").asScala).flatten.toSet
          } yield RawUserAccount(encodedPassword = encodedPassword, permissions = permissions)
      }

    new IdToUser(userIdToRaw, toUser, toPermission)
  }

  private val sha512Hasher = { o: String => sha512.hashString(o: String, UTF_8).toString } withToString "sha512"
  private val identityHasher = { o: String => identity(o) } withToString "identity"

  private def toHashedPassword(userId: UserId, encodedPassword: SecretString) =
    encodedPassword.string match {
      case EntryRegex("plain", pw) =>
        Some(HashedPassword(SecretString(pw), identityHasher))

      case EntryRegex("sha512", pw) =>
        Some(HashedPassword(SecretString(pw), sha512Hasher))

      case EntryRegex(_, _) =>
        logger.error(s"Unknown password encoding scheme for User '$userId'")
        None

      case _ =>
        logger.error(s"Missing password encoding scheme for User '$userId'. Try to prefix the configured password with 'plain:' or 'sha512:'")
        None
    }

  private[auth] final case class RawUserAccount(encodedPassword: SecretString, permissions: Set[String])
}
