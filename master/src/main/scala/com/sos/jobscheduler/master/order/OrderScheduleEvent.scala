package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.data.event.NoKeyEvent
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderScheduleEvent extends NoKeyEvent

object OrderScheduleEvent {
  final case class GeneratedUntil(until: Instant)
  extends OrderScheduleEvent

  implicit val jsonFormat = TypedJsonFormat[OrderScheduleEvent](
    Subtype(jsonFormat1(GeneratedUntil), "OrderScheduleEvent.GeneratedUntil"))
}
