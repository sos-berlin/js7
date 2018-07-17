package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.event.{EventId, JournalStateEvent}

/**
  * @author Joacim Zschimmer
  */
private[journal] final case class JournalState(eventsAcceptedUntil: EventId)
{
  def handleEvent(event: JournalStateEvent): JournalState =
    event match {
      case JournalStateEvent.EventsAccepted(untilEventId) ⇒
        copy(eventsAcceptedUntil = untilEventId)
    }
}

private[journal] object JournalState
{
  val empty = new JournalState(EventId.BeforeFirst)
  implicit val jsonCodec = TypedJsonCodec[JournalState](
    Subtype(deriveCodec[JournalState])
  )
}
