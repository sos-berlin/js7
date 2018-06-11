package com.sos.jobscheduler.base.auth

import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.generic.SecretString.implicits.{JsonDecoder, JsonEncoder}

/**
  * @author Joacim Zschimmer
  */
final case class SessionToken(secret: SecretString) {
  override def toString = "SessionToken"
}

object SessionToken {
  (JsonEncoder, JsonDecoder)  // For IntelliJ import

  val HeaderName = "X-JobScheduler-Session"
}
