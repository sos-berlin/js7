package com.sos.scheduler.engine.data.event

/**
  * @author Joacim Zschimmer
  */
trait Event

object Event {
  // JsonFormat is placed in  // Use events.EventJsonFormat to break package dependency cycle
  //implicit val EventJsonFormat: TypedJsonFormat[Event] =
}
