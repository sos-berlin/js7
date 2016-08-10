package com.sos.scheduler.engine.data

import com.sos.scheduler.engine.base.sprayjson.TypedJsonFormat
import com.sos.scheduler.engine.base.sprayjson.TypedJsonFormat.Subtype
import com.sos.scheduler.engine.data.event.Event
import com.sos.scheduler.engine.data.order.OrderEvent

/**
  * @author Joacim Zschimmer
  */
package object events {

  implicit val EventJsonFormat = TypedJsonFormat[Event](
    Subtype[OrderEvent])
}
