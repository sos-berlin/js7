package js7.common.pekkohttp.web.auth

import org.apache.pekko.http.scaladsl.server.directives.Credentials
import org.apache.pekko.http.scaladsl.server.directives.SecurityDirectives.Authenticator
import js7.base.auth.{User, UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.utils.Memoizer
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.pekkohttp.web.auth.OurMemoizingAuthenticator.*

/**
  * Rejects authentication (returning None) only if provided credentials are invalid or user is `Anonymous`.
  * If the authentication is missing, `Anomymous` is returnend.
  *
  * @author Joacim Zschimmer
  */
final class OurMemoizingAuthenticator[U <: User](toUser: UserId => Option[U])
extends Authenticator[U]:

  private val memoizedToUser = Memoizer.strict1(toUser)  // Only cache for short time if source will be a changing database !!!

  def apply(credentials: Credentials) =
    credentials match
      case provided: Credentials.Provided =>
        val userId = UserId(provided.identifier)
        userId match
          case UserId.Anonymous =>
            None  // Anonymous is only for internal use, not for explicit authentication
          case _ =>
            memoizedToUser(userId) match
              case Some(user) =>
                provided.verify(user.hashedPassword.hashed.string, user.hashedPassword.hasher) ? user
              case None =>
                None

      case Credentials.Missing =>
        authenticate(AnonymousAndPassword)

  def authenticate(userAndPassword: UserAndPassword): Option[U] =
    memoizedToUser(userAndPassword.userId)
      .filter(_.hashedPassword equalsClearText userAndPassword.password)


object OurMemoizingAuthenticator:
  private val AnonymousAndPassword = UserAndPassword(UserId.Anonymous -> SecretString.empty)
