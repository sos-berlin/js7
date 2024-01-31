package js7.provider.scheduledorder

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.time.Timestamp
import js7.data.event.NoKeyEvent

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderScheduleEvent extends NoKeyEvent


object OrderScheduleEvent:
  final case class GeneratedUntil(until: Timestamp)
  extends OrderScheduleEvent
  object GeneratedUntil:
    implicit val jsonCodec: Codec.AsObject[GeneratedUntil] = deriveCodec

  implicit val jsonCodec: TypedJsonCodec[OrderScheduleEvent] = TypedJsonCodec(
    Subtype.named1[GeneratedUntil]("OrderScheduleEvent.GeneratedUntil"))
