package com.sos.scheduler.engine.http.client.heartbeat

import com.sos.scheduler.engine.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.http.client.common.{OwnHttpHeader, OwnHttpHeaderCompanion}
import spray.http.HttpHeader

/**
  * @author Joacim Zschimmer
  */
object HeartbeatResponseHeaders {

  final case class `X-JobScheduler-Heartbeat`(heartbeatId: HeartbeatId) extends OwnHttpHeader {
    def companion = `X-JobScheduler-Heartbeat`
    def value = s"${heartbeatId.string}"
  }
  object `X-JobScheduler-Heartbeat` extends OwnHttpHeaderCompanion {
    def unapply(h: HttpHeader): Option[HeartbeatId] =
      h.name.toLowerCase == lowercaseName option {
        val Value(id) = h.value
        id
      }

    object Value {
      def unapply(value: String): Some[HeartbeatId] = Some(HeartbeatId(value))
    }
  }
}
