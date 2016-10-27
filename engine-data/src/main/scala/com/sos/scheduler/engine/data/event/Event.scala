package com.sos.scheduler.engine.data.event

/**
  * An object's event.
  * <p>
  *   See [[KeyedEvent]] for the key of object.
  *
  * @author Joacim Zschimmer
  */
trait Event {
  /**
    * The type of the key in [[KeyedEvent]] or [[KeyedEvent.NoKey]].
    */
  type Key
}
