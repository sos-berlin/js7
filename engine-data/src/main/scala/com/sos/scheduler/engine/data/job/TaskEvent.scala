package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.event.Event
import spray.json.DefaultJsonProtocol._

sealed trait TaskEvent extends Event {
  type Key = TaskKey
}

object TaskEvent {
  implicit val jsonFormat = TypedJsonFormat[TaskEvent](
    Subtype(jsonFormat0(() ⇒ TaskStarted)),
    Subtype(jsonFormat1(TaskEnded)),
    Subtype(jsonFormat0(() ⇒ TaskClosed)))
}

case object TaskStarted
extends TaskEvent

/**
  * @param returnCode The JobScheduler Task `exit_code`.
  */
final case class TaskEnded(returnCode: ReturnCode)
extends TaskEvent

/**
  * Anything is cleaned up (like temporary files).
  */
case object TaskClosed
extends TaskEvent
