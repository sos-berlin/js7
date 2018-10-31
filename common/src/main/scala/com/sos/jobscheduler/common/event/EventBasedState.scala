package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.data.event.EventId

/**
  * @author Joacim Zschimmer
  */
trait EventBasedState {
  def eventId: EventId
}
