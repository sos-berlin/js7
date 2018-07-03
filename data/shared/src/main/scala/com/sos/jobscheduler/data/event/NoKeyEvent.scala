package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
trait NoKeyEvent extends Event {
  final type Key = NoKey
}

object NoKeyEvent {
  implicit def toKeyedEvent[E <: NoKeyEvent](event: E): KeyedEvent[E] =
    KeyedEvent(event)
}
