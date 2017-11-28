package com.sos.jobscheduler.master.gui.common

import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.order.{OrderEvent, OrderId}

/**
  * @author Joacim Zschimmer
  */
object Utils {

  def eventToLog(eventId: EventId, orderId: OrderId, event: OrderEvent) =
    s"${EventId.toString(eventId)} $orderId ${event.getClass.getSimpleName}"
}
