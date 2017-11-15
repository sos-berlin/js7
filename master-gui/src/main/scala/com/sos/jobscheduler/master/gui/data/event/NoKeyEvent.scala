package com.sos.jobscheduler.master.gui.data.event

import com.sos.jobscheduler.master.gui.data.event.KeyedEvent.NoKey
import scala.language.implicitConversions

trait NoKeyEvent extends Event {
  final type Key = NoKey
}

object NoKeyEvent {
  implicit def toKeyedEvent(e: NoKeyEvent): KeyedEvent[NoKeyEvent] =
    KeyedEvent(e)
}
