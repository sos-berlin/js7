package js7.data.workflow

import io.circe.generic.JsonCodec
import js7.data.event.{Event, NoKeyEvent}

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
