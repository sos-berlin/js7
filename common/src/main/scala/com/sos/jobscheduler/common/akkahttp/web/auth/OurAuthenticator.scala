package com.sos.jobscheduler.common.akkahttp.web.auth

import akka.http.scaladsl.server.directives.Credentials
import akka.http.scaladsl.server.directives.SecurityDirectives.Authenticator
import com.sos.jobscheduler.base.auth.{SimpleUser, User, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.common.auth.HashedPassword

/**
  * @author Joacim Zschimmer
  */
final class OurAuthenticator(userIdToHashedPassword: UserId ⇒ Option[HashedPassword])
extends Authenticator[User] {

  def apply(credentials: Credentials) =
    credentials match {
      case provided: Credentials.Provided ⇒
        val userId = UserId(provided.identifier)
        userId match {
          //case UserId.Empty ⇒
          //  validateAccessToken.lift(provided.secret) map SimpleUser.apply   No way to access the provided password ???
          case _ ⇒
            userIdToHashedPassword(userId) match {
              case Some(HashedPassword(SecretString(hashed), hasher)) ⇒
                provided.verify(hashed, hasher) option SimpleUser(userId)
              case None ⇒ None
            }
        }

      case _ ⇒ None
    }
}
