package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait TaskObstacle

object TaskObstacle {
  case object WaitingForLock
  extends TaskObstacle

  case object WaitingForProcessClass
  extends TaskObstacle

  case object WaitingForAgent
  extends TaskObstacle

  case object Delayed
  extends TaskObstacle

  case object Suspended
  extends TaskObstacle

  implicit val MyJsonFormat = TypedJsonFormat[TaskObstacle](
    Subtype(jsonFormat0(() ⇒ WaitingForLock)),
    Subtype(jsonFormat0(() ⇒ WaitingForProcessClass)),
    Subtype(jsonFormat0(() ⇒ WaitingForAgent)),
    Subtype(jsonFormat0(() ⇒ Delayed)),
    Subtype(jsonFormat0(() ⇒ Suspended)))
}
