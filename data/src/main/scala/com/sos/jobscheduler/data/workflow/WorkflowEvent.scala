package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.data.event.Event
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait WorkflowEvent extends Event {
  type Key = WorkflowPath
}

object WorkflowEvent {
  final case class WorkflowAttached(inputNodeId: NodeId, idToNode: Map[NodeId, Workflow.Node])
  extends WorkflowEvent

  //TODO case object WorkflowDeleted   Wann wird ein Workflowz vom AgentOrderKeeper gelöscht?

  implicit val jsonType = TypedJsonFormat[WorkflowEvent](
    Subtype(jsonFormat2(WorkflowAttached))
  )
}
