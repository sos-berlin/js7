package com.sos.scheduler.engine.common.sprayutils.web.auth

import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.auth.{SimpleUser, User, UserAndPassword, UserId}
import scala.concurrent.Future
import spray.routing.authentication._

/**
  * @author Joacim Zschimmer
  */
final class SimpleUserPassAuthenticator(
  isValidPassword: UserAndPassword ⇒ Boolean,
  validateAccessToken: PartialFunction[SecretString, UserId])
extends UserPassAuthenticator[User] {

  def apply(userPass: Option[UserPass]) =
    Future.successful(
      userPass flatMap { case UserPass(user, pass) ⇒
        val password = SecretString(pass)
        UserId(user) match {
          case UserId.Empty ⇒
            validateAccessToken.lift(password) map SimpleUser.apply
          case userId if isValidPassword(UserAndPassword(userId, password)) ⇒
            Some(SimpleUser(userId))
          case _ ⇒ None
        }
      })
}
