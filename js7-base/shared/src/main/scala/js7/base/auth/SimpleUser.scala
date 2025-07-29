package js7.base.auth

import js7.base.auth.SimpleUser.*
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*

/**
  * @author Joacim Zschimmer
  */
final case class SimpleUser(
  id: UserId,
  hashedPassword: HashedPassword,
  grantedPermissions: Set[Permission],
  distinguishedNames: Seq[DistinguishedName] = Nil)
extends User:

  logger.trace(s"SimpleUser($id, $grantedPermissions, $distinguishedNames)")

  override def toString = s""

  if id.isAnonymous && grantedPermissions.contains(ValidUserPermission) then
    throw new IllegalArgumentException("Anonymous must not have ValidUserPermission")
  // SuperPermission is allowed for empowered Anonymous (public = on | loopback-is-public = on)


object SimpleUser extends User.Companion[SimpleUser]:
  private val logger = Logger[this.type]

  /** The unauthenticated, anonymous user without permissions, for testing. */
  val TestAnonymous: SimpleUser =
    SimpleUser(UserId.Anonymous, HashedPassword.newEmpty())

  val System: SimpleUser =
    SimpleUser(UserId("System"), HashedPassword.MatchesNothing, Set(SuperPermission))

  implicit val companion: User.Companion[SimpleUser] = this

  def addPermissions(user: SimpleUser, permissions: Set[Permission]): SimpleUser =
    reuseIfEqual(user, user.copy(
      grantedPermissions = user.grantedPermissions ++ permissions))

  def apply(
    id: UserId,
    hashedPassword: HashedPassword = HashedPassword.MatchesNothing,
    grantedPermissions: Set[Permission] = Set.empty,
    distinguishedNames: Seq[DistinguishedName] = Nil)
  : SimpleUser =
    new SimpleUser(
      id,
      hashedPassword,
      grantedPermissions ++ (!id.isAnonymous thenSet ValidUserPermission),
      distinguishedNames)
