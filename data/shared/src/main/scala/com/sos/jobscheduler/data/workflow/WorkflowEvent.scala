package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.event.{Event, NoKeyEvent}
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
sealed trait WorkflowEvent extends Event

object WorkflowEvent {
  @JsonCodec
  final case class WorkflowAttached(workflow: Workflow)
  extends WorkflowEvent with NoKeyEvent

  //TODO case object WorkflowDeleted   Wann wird ein Workflow vom AgentOrderKeeper gelöscht?

  //implicit val jsonCodec = TypedJsonCodec[WorkflowEvent](
  //  Subtype[WorkflowAttached])
}
