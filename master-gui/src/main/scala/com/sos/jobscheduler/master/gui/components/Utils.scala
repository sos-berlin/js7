package com.sos.jobscheduler.master.gui.components

import com.sos.jobscheduler.master.gui.data.event.EventId
import com.sos.jobscheduler.master.gui.data.{OrderEvent, OrderId}

/**
  * @author Joacim Zschimmer
  */
object Utils {

  def eventToLog(eventId: EventId, orderId: OrderId, event: OrderEvent) =
    s"${EventId.toString(eventId)} $orderId ${event.getClass.getSimpleName}"
}
