package com.sos.jobscheduler.base.auth

import com.sos.jobscheduler.base.generic.SecretString

/**
  * @author Joacim Zschimmer
  */
final case class SessionToken(secret: SecretString) {
  override def toString = "SessionToken"
}

object SessionToken {
  val HeaderName = "X-JobScheduler-Session"
}
