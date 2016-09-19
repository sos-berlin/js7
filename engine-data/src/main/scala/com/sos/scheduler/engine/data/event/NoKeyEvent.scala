package com.sos.scheduler.engine.data.event

import com.sos.scheduler.engine.data.event.KeyedEvent.NoKey
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
trait NoKeyEvent extends Event {
  final type Key = NoKey.type
}

object NoKeyEvent {
  implicit def toKeyedEvent(e: NoKeyEvent): KeyedEvent[NoKeyEvent] =
    KeyedEvent(e)
}
