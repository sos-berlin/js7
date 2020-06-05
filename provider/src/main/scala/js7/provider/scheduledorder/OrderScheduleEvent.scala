package js7.provider.scheduledorder

import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.time.Timestamp
import js7.data.event.NoKeyEvent
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderScheduleEvent extends NoKeyEvent

object OrderScheduleEvent {
  @JsonCodec
  final case class GeneratedUntil(until: Timestamp)
  extends OrderScheduleEvent

  implicit val jsonCodec = TypedJsonCodec[OrderScheduleEvent](
    Subtype.named[GeneratedUntil]("OrderScheduleEvent.GeneratedUntil"))
}
