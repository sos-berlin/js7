package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.common.auth.UserId
import spray.json.DefaultJsonProtocol.jsonFormat1

/**
  * @author Joacim Zschimmer
  */
private[scheduler] sealed trait AgentSnapshot

private[scheduler] object AgentSnapshot {
  final case class Master(userId: UserId) extends AgentSnapshot

  val jsonFormat = TypedJsonFormat[Any](
    Subtype(jsonFormat1(Master.apply)))
}
