package com.sos.scheduler.engine.http.client.heartbeat

import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.http.client.common.{OwnHttpHeader, OwnHttpHeaderCompanion}
import java.time.Duration

/**
  * @author Joacim Zschimmer
  */
object HeartbeatRequestHeaders {

  private val TimesRegex = s"($Iso8601DurationRegex) +($Iso8601DurationRegex)".r

  private def parseTimes(string: String) = string match {
    case TimesRegex(duration, timeout) ⇒ HttpHeartbeatTiming(Duration parse duration, Duration parse timeout)
  }

  final case class `X-JobScheduler-Heartbeat-Start`(timing: HttpHeartbeatTiming) extends OwnHttpHeader {
    def companion = `X-JobScheduler-Heartbeat-Start`
    def value = s"${timing.period} ${timing.timeout}"
  }
  object `X-JobScheduler-Heartbeat-Start` extends OwnHttpHeaderCompanion {
    object Value {
      def unapply(value: String): Some[HttpHeartbeatTiming] = Some(parseTimes(value))
    }
  }

 final case class `X-JobScheduler-Heartbeat-Continue`(heartbeatId: HeartbeatId, timing: HttpHeartbeatTiming) extends OwnHttpHeader {
    def companion = `X-JobScheduler-Heartbeat-Continue`
    def value = s"${heartbeatId.string} ${timing.period} ${timing.timeout}"
  }
  object `X-JobScheduler-Heartbeat-Continue` extends OwnHttpHeaderCompanion {
    private val ValueRegex = s"(${HeartbeatId.Regex}) +($Iso8601DurationRegex +$Iso8601DurationRegex)".r

    object Value {
      def unapply(value: String): Option[(HeartbeatId, HttpHeartbeatTiming)] =
        value match {
          case ValueRegex(heartbeatId, timing) ⇒ Some((HeartbeatId(heartbeatId), parseTimes(timing)))
        }
      }
  }

 final case class `X-JobScheduler-Heartbeat`(timing: HttpHeartbeatTiming) extends OwnHttpHeader {
    def companion = `X-JobScheduler-Heartbeat`
    def value = s"${timing.period} ${timing.timeout}"
  }
  object `X-JobScheduler-Heartbeat` extends OwnHttpHeaderCompanion {
    object Value {
      def unapply(value: String): Some[HttpHeartbeatTiming] = Some(parseTimes(value))
    }
  }
}
