package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.event.NoKeyEvent
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderScheduleEvent extends NoKeyEvent

object OrderScheduleEvent {
  @JsonCodec
  final case class GeneratedUntil(until: Timestamp)
  extends OrderScheduleEvent

  implicit val JsonCodec = TypedJsonCodec[OrderScheduleEvent](
    Subtype.named[GeneratedUntil]("OrderScheduleEvent.GeneratedUntil"))
}
