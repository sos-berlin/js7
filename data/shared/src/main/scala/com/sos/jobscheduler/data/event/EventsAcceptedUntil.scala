package com.sos.jobscheduler.data.event

import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class EventsAcceptedUntil(untilEventId: EventId) extends NoKeyEvent
