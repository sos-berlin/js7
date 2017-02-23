package com.sos.scheduler.engine.http.client.idempotence

import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.http.client.common.{OwnHttpHeader, OwnHttpHeaderCompanion}
import java.time.Duration

/**
  * @author Joacim Zschimmer
  */
object IdempotentHeaders {

  final case class `X-JobScheduler-Request-ID`(id: RequestId, lifetime: Option[Duration] = None) extends OwnHttpHeader {
    def companion = `X-JobScheduler-Request-ID`
    def value = s"${id.number} ${lifetime getOrElse `X-JobScheduler-Request-ID`.EternalLifetime}"
  }

  object `X-JobScheduler-Request-ID` extends OwnHttpHeaderCompanion {
    val EternalLifetime = Duration.ofSeconds(999999999)
    private val ValueRegex = s"(\\d+) ($Iso8601DurationRegex)".r

    object Value {
      def unapply(value: String): Option[(RequestId, Option[Duration])] =
        value match {
          case ValueRegex(id, duration) ⇒
            val lifetimeOption = Duration.parse(duration) match {
              case EternalLifetime ⇒ None
              case o ⇒ Some(o)
            }
            Some((RequestId.fromString(id), lifetimeOption))
        }
      }
  }
}
