package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
private[scheduler] sealed trait AgentSnapshot

private[scheduler] object AgentSnapshot {
  @JsonCodec
  final case class Master(userId: UserId) extends AgentSnapshot

  val jsonCodec = TypedJsonCodec[Any](
    Subtype[Master])
}
