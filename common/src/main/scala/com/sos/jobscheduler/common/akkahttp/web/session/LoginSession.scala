package com.sos.jobscheduler.common.akkahttp.web.session

import com.sos.jobscheduler.base.auth.{SimpleUser, User ⇒ User_}

/**
  * @author Joacim Zschimmer
  */
trait LoginSession
{
  type User <: User_

  protected def sessionInit: SessionInit[User]

  final def sessionNumber = sessionInit.sessionNumber

  final def sessionToken = sessionInit.sessionToken

  final def user = sessionInit.user
}

object LoginSession {
  final case class Simple(sessionInit: SessionInit[SimpleUser]) extends LoginSession {
    type User = SimpleUser
  }
}
