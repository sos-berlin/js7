package com.sos.scheduler.engine.data.event

/**
  * @author Joacim Zschimmer
  */
trait Event {
  /**
    * The type of the key in [[KeyedEvent]] or [[KeyedEvent.NoKey]].
    */
  type Key
}
