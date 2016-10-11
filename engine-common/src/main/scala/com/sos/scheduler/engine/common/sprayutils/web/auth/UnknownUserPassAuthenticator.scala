package com.sos.scheduler.engine.agent.web.auth

import com.sos.scheduler.engine.common.auth.{Account, UnknownAccount}
import scala.concurrent.Future
import spray.routing.authentication.{UserPass, _}

/**
  * Returns `UnknownAccount` for requests with or without credentials.
  *
  * @author Joacim Zschimmer
  */
object UnknownUserPassAuthenticator extends UserPassAuthenticator[Account] {

  def apply(userPass: Option[UserPass]) = Future.successful(Some(UnknownAccount))
}
