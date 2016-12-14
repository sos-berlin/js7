package com.sos.scheduler.engine.data.scheduler

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.event.NoKeyEvent
import com.sos.scheduler.engine.data.scheduler.SchedulerStates._
import spray.json.DefaultJsonProtocol._

/**
  * Event from [[com.sos.scheduler.engine.main.SchedulerThreadController]]
  * or [[com.sos.scheduler.engine.main.SchedulerThread]]. */
sealed trait SchedulerEvent extends NoKeyEvent

object SchedulerEvent {
  implicit val jsonFormat: TypedJsonFormat[SchedulerEvent] =
    TypedJsonFormat[SchedulerEvent](
      Subtype(jsonFormat1(SchedulerStateChanged)),
      Subtype(jsonFormat0(() ⇒ SchedulerInitiated)),
      Subtype(jsonFormat0(() ⇒ SchedulerClosed)))
}

final case class SchedulerStateChanged(state: SchedulerState) extends SchedulerEvent

case object SchedulerInitiated extends SchedulerEvent

/**
  * JavaSubsystem has been closed.
  */
case object SchedulerClosed extends SchedulerEvent

final class SchedulerTerminatedEvent(exitCode: Int, throwableOption: Option[Throwable])
extends SchedulerEvent
