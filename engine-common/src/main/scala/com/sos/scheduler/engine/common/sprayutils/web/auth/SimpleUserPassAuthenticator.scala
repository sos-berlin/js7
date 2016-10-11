package com.sos.scheduler.engine.agent.web.auth

import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.auth.{Account, SimpleAccount, UserAndPassword}
import scala.concurrent.Future
import spray.routing.authentication._

/**
  * @author Joacim Zschimmer
  */
final class SimpleUserPassAuthenticator(isValidPassword: UserAndPassword ⇒ Boolean)
extends UserPassAuthenticator[Account] {

  def apply(userPass: Option[UserPass]) =
    Future.successful(
      userPass match {
        case Some(UserPass(user, password)) if isValidPassword(UserAndPassword(user, SecretString(password))) ⇒
          Some(SimpleAccount(user))
        case _ ⇒
          None
      })
}
