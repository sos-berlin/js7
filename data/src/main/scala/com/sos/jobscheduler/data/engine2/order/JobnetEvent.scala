package com.sos.jobscheduler.data.engine2.order

import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.data.engine2.order.Jobnet.Node
import com.sos.jobscheduler.data.event.Event
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait JobnetEvent extends Event {
  type Key = JobnetPath
}

object JobnetEvent {
  final case class JobnetAttached(inputNodeId: NodeId, idToNode: Map[NodeId, Node])
  extends JobnetEvent

  implicit val jsonType = TypedJsonFormat[JobnetEvent](
    Subtype(jsonFormat2(JobnetAttached))
  )
}
