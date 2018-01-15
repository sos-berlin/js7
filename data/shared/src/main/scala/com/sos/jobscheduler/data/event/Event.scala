package com.sos.jobscheduler.data.event

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

  @deprecated("Use <-:", "")  // For easier typing. When typing <-:, IntelliJ replaces it by ←:
  final def <<:(key: Key) = key <-: this

  final def <-:(key: Key) = new KeyedEvent[this.type](key, this)
}
