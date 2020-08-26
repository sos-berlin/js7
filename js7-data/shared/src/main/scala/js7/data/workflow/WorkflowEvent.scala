package js7.data.workflow

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.data.event.{Event, NoKeyEvent}

/**
  * @author Joacim Zschimmer
  */
sealed trait WorkflowEvent extends Event

object WorkflowEvent
{
  final case class WorkflowAttached(workflow: Workflow)
  extends WorkflowEvent with NoKeyEvent
  object WorkflowAttached {
    implicit val jsonCodec = deriveCodec[WorkflowAttached]
  }

  //TODO case object WorkflowDeleted   Wann wird ein Workflow vom AgentOrderKeeper gelöscht?

  //implicit val jsonCodec = TypedJsonCodec[WorkflowEvent](
  //  Subtype[WorkflowAttached])
}
