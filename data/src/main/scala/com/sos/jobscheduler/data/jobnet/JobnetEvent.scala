package com.sos.jobscheduler.data.jobnet

import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.data.event.Event
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait JobnetEvent extends Event {
  type Key = JobnetPath
}

object JobnetEvent {
  final case class JobnetAttached(inputNodeId: NodeId, idToNode: Map[NodeId, Jobnet.Node])
  extends JobnetEvent

  //TODO case object JobnetDeleted   Wann wird ein Jobnetz vom AgentOrderKeeper gelöscht?

  implicit val jsonType = TypedJsonFormat[JobnetEvent](
    Subtype(jsonFormat2(JobnetAttached))
  )
}
