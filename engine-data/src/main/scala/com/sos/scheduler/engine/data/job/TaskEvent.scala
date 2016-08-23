package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.data.event.Event

sealed trait TaskEvent extends Event {
  type Key = TaskKey
}

case object TaskStarted
extends TaskEvent

final case class TaskEnded(returnCode: ReturnCode)
extends TaskEvent

case object TaskClosed
extends TaskEvent
