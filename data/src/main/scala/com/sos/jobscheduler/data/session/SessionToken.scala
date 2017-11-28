package com.sos.jobscheduler.data.session

import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.generic.SecretString.implicits.{JsonDecoder, JsonEncoder}
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class SessionToken(secret: SecretString) {
  override def toString = "SessionToken"
}

object SessionToken {
  (JsonEncoder, JsonDecoder)  // For IntelliJ import

  val HeaderName = "X-JobScheduler-Session"
}
