package com.sos.scheduler.engine.common.sprayutils.web.auth

import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.auth.{SimpleUser, User, UserAndPassword, UserId}
import scala.concurrent.Future
import spray.routing.authentication._

/**
  * @author Joacim Zschimmer
  */
final class SimpleUserPassAuthenticator(
  isValidPassword: UserAndPassword ⇒ Boolean)
extends UserPassAuthenticator[User] {

  def apply(userPass: Option[UserPass]) =
    Future.successful(
      userPass match {
        case Some(UserPass(user, password)) if isValidPassword(UserAndPassword(UserId(user), SecretString(password))) ⇒
          Some(SimpleUser(UserId(user)))
        case _ ⇒
          None
      })
}

object SimpleUserPassAuthenticator {
  val AccessTokenPseudoUser = ""
}
