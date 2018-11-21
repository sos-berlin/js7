package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.data.event.EventId

/**
  * @author Joacim Zschimmer
  */
final class TornException private[event](val after: EventId, val tornEventId: EventId)
extends RuntimeException {
  override def getMessage = s"EventSeq is torn - after=$after tornEventId=$tornEventId"
}
