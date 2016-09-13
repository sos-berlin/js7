package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait TaskObstacle

object TaskObstacle {
  case object LockUnavailable
  extends TaskObstacle

  case object ProcessClassUnavailable
  extends TaskObstacle

  case object AgentUnavailable
  extends TaskObstacle

  case object Delayed
  extends TaskObstacle

  case object Suspended
  extends TaskObstacle

  implicit val MyJsonFormat = TypedJsonFormat[TaskObstacle](
    Subtype(jsonFormat0(() ⇒ LockUnavailable)),
    Subtype(jsonFormat0(() ⇒ ProcessClassUnavailable)),
    Subtype(jsonFormat0(() ⇒ AgentUnavailable)),
    Subtype(jsonFormat0(() ⇒ Delayed)),
    Subtype(jsonFormat0(() ⇒ Suspended)))
}
