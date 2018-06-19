package com.sos.jobscheduler.common.akkahttp.web.session

import com.sos.jobscheduler.base.auth.{User ⇒ User_}

/**
  * @author Joacim Zschimmer
  */
trait Session extends HasTimeout
{
  type User <: User_

  protected def sessionInit: SessionInit[User]

  final def sessionNumber = sessionInit.sessionNumber

  final def sessionToken = sessionInit.sessionToken

  final def user = sessionInit.user
}
