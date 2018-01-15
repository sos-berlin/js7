package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.event.Event
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
sealed trait WorkflowEvent extends Event {
  type Key = WorkflowPath
}

object WorkflowEvent {
  @JsonCodec
  final case class WorkflowAttached(workflow: Workflow)
  extends WorkflowEvent

  //TODO case object WorkflowDeleted   Wann wird ein Workflow vom AgentOrderKeeper gelöscht?

  implicit val jsonCodec = TypedJsonCodec[WorkflowEvent](
    Subtype[WorkflowAttached])
}
