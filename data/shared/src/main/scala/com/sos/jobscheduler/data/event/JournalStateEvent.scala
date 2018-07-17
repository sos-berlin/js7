package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
sealed trait JournalStateEvent extends NoKeyEvent

object JournalStateEvent
{
  @JsonCodec
  final case class EventsAccepted(untilEventId: EventId)
  extends JournalStateEvent

  implicit val jsonCodec = TypedJsonCodec[JournalStateEvent](
    Subtype(deriveCodec[EventsAccepted]))
}
