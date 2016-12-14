package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.event.Event
import com.sos.scheduler.engine.data.jobchain.{JobChainNodeAction, JobChainPath, JobChainState, NodeId}
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait JobChainEvent extends Event {
  type Key = JobChainPath
}

object JobChainEvent {
  private implicit val x = JobChainState.jsonFormat
  private implicit val y = JobChainNodeAction.MyJsonFormat

  implicit val jsonFormat = TypedJsonFormat[JobChainEvent](
    Subtype(jsonFormat1(JobChainStateChanged)),
    Subtype(jsonFormat2(JobChainNodeActionChanged)))
}

final case class JobChainStateChanged(state: JobChainState)
extends JobChainEvent

trait JobChainNodeEvent extends JobChainEvent {
  def nodeId: NodeId
}

final case class JobChainNodeActionChanged(nodeId: NodeId, action: JobChainNodeAction)
extends JobChainNodeEvent
