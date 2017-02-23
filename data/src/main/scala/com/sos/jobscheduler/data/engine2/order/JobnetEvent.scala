package com.sos.scheduler.engine.data.engine2.order

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.engine2.order.JobNet.Node
import com.sos.scheduler.engine.data.event.Event
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait JobnetEvent extends Event {
  type Key = JobChainPath
}

object JobnetEvent {
  final case class JobnetAttached(inputNodeId: NodeId, idToNode: Map[NodeId, Node])
  extends JobnetEvent

  implicit val jsonType = TypedJsonFormat[JobnetEvent](
    Subtype(jsonFormat2(JobnetAttached))
  )
}
