package js7.common.auth

import com.typesafe.config.{Config, ConfigObject}
import java.util.Locale
import js7.base.auth.{DistinguishedName, HashedPassword, Permission, User, UserId}
import js7.base.configutils.Configs.*
import js7.base.generic.SecretString
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Memoizer
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.auth.IdToUser.*
import scala.jdk.CollectionConverters.*
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
  distinguishedNameToUserIds: DistinguishedName => Set[UserId],
  toUser: (UserId, HashedPassword, Set[Permission], Seq[DistinguishedName]) => U,
  toPermission: PartialFunction[String, Permission])
extends (UserId => Option[U]):
  private lazy val someAnonymous = Some(toUser(UserId.Anonymous, HashedPassword.newEmpty(), Set.empty, Nil))
  private val memoizedToUser = Memoizer.strict1((userId: UserId) =>
    if userId.isAnonymous then
      someAnonymous
    else
      userIdToRaw(userId).flatMap(rawToUser))

  def apply(userId: UserId): Option[U] = memoizedToUser(userId)

  def distinguishedNameToIdsOrUser(distinguishedName: DistinguishedName): Checked[Either[Set[UserId], U]] =
    val userIds = distinguishedNameToUserIds(distinguishedName)
    def unknownDN = Problem(s"Unknown distinguished name '$distinguishedName'")
    if userIds.isEmpty then
      Left(unknownDN)
    else if userIds.sizeIs == 1 then
      apply(userIds.head) match
        case None => Left(unknownDN)  // should not happen
        case Some(user) => Right(Right(user))   // the authenticated user
    else
      assert(userIds.sizeIs > 1)
      Right(Left(userIds))  // Only one of these UserIds is allowed to authenticate

  private def rawToUser(raw: RawUserAccount): Option[U] =
    (raw.encodedPassword match {
      case None => Some(HashedPassword.MatchesNothing)
      case Some(pw) => toHashedPassword(raw.userId, pw).map(_.hashAgainRandom)
    }).map(hashedPassword => toUser(
        raw.userId,
        hashedPassword,
        raw.permissions.flatMap(toPermission.lift),
        raw.distinguishedNames))

object IdToUser:
  private val logger = Logger[this.type]
  private val UsersConfigPath = "js7.auth.users"
  private val PasswordRegex = "([^:]+):(.*)".r

  def fromConfig[U <: User](
    config: Config,
    toUser: (UserId, HashedPassword, Set[Permission], Seq[DistinguishedName]) => U,
    toPermission: PartialFunction[String, Permission] = PartialFunction.empty)
  : IdToUser[U] =
    val cfg = config.getConfig(UsersConfigPath)
    val cfgObject = config.getValue(UsersConfigPath).asInstanceOf[ConfigObject]

    def userIdToRaw(userId: UserId): Option[RawUserAccount] =
      if cfg.hasPath(userId.string) then
        existentUserIdToRaw(userId)
      else
        logger.debug(
          s"""Configuration files ("private.conf") does not have an entry '$UsersConfigPath.${userId.string}'""")
        None

    def existentUserIdToRaw(userId: UserId): Option[RawUserAccount] =
      Try(cfg.getConfig(userId.string)) match
        case Failure(_: com.typesafe.config.ConfigException.WrongType) =>  // Entry is not a configuration object {...} but a string (the password)
          cfg.optionAs[SecretString](userId.string).map(o =>
            RawUserAccount(userId, encodedPassword = Some(o)))

        case Failure(t) =>
          throw t

        case Success(c) =>
          val encodedPassword = c.optionAs[SecretString]("password")
          val permissions = c.stringSeq("permissions", Nil).toSet
          val distinguishedNames = c.seqAs[DistinguishedName]("distinguished-names", Nil)
          Some(RawUserAccount(userId, encodedPassword = encodedPassword, permissions = permissions,
            distinguishedNames))

    val distinguishedNameToUserIds: Map[DistinguishedName, Set[UserId]] =
      cfgObject.asScala.view
        .flatMap { case (key, value) =>
          UserId.checked(key)
            .toOption/*ignore error*/
            .view
            .flatMap(userId =>
              value match {
                case value: ConfigObject =>
                  Option(value.get("distinguished-names"))
                    .view
                    .flatMap(_.unwrapped match {
                      case list: java.util.List[?] =>
                        list.asScala.flatMap {
                          case o: String =>
                            DistinguishedName.checked(o)
                              .toOption/*ignore error*/
                              .map(_ -> userId)
                          case _ =>
                            Nil/*ignore type error*/
                        }
                    })
                case _=>
                  Nil/*ignore type mismatch*/
              })
        }
        .groupMap(_._1)(_._2)
        .map { case (k, v) => k -> v.toSet }

    logger.debug(s"distinguishedNameToUserIds=⏎${
      distinguishedNameToUserIds.map { case (k, v) => s"\n  $k --> ${v mkString " "}" }.mkString}")
    assert(distinguishedNameToUserIds.values.forall(_.nonEmpty))  // Set[UserId] is not empty

    new IdToUser(userIdToRaw, dn => distinguishedNameToUserIds.getOrElse(dn, Set.empty),
      toUser, toPermission)

  private val identityHasher: String => String =
    ((o: String) => identity(o)).withToString("identity")

  private def toHashedPassword(userId: UserId, encodedPassword: SecretString) =
    encodedPassword.string match
      case PasswordRegex("plain", pw) =>
        Some(HashedPassword(SecretString(pw), identityHasher))

      case PasswordRegex("sha512", pw) =>
        Some(HashedPassword(SecretString(pw.toLowerCase(Locale.ROOT)), Hasher.sha512))

      case "" =>
        Some(HashedPassword(SecretString.empty, identityHasher))

      case PasswordRegex(_, _) =>
        logger.error(s"Unknown password encoding scheme for User '$userId'")
        None

      case _ =>
        logger.error(s"Missing password encoding scheme for User '$userId'. Try to prefix the configured password with 'plain:' or 'sha512:'")
        None

  private[auth] final case class RawUserAccount(
    userId: UserId,
    encodedPassword: Option[SecretString],
    permissions: Set[String] = Set.empty,
    distinguishedNames: Seq[DistinguishedName] = Nil)
