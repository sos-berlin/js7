package js7.controller.data.agent

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.problem.{Checked, Problem}
import js7.controller.data.events.AgentRefStateEvent
import js7.controller.data.events.AgentRefStateEvent.{AgentCouplingFailed, AgentEventsObserved, AgentReady, AgentRegisteredController}
import js7.data.agent.{AgentRef, AgentRunId}
import js7.data.event.EventId

final case class AgentRefState(agentRef: AgentRef, agentRunId: Option[AgentRunId], eventId: EventId)
{
  def name = agentRef.name

  def applyEvent(event: AgentRefStateEvent): Checked[AgentRefState] =
    event match {
      case AgentRegisteredController(agentRunId_) =>
        if (agentRunId.isDefined || eventId != EventId.BeforeFirst)
          Left(Problem("Duplicate AgentRegisteredController event: " + event))
        else
          Right(copy(agentRunId = Some(agentRunId_)))

      case _: AgentReady | _: AgentCouplingFailed =>
        Right(this)

      case AgentEventsObserved(eventId_) =>
        if (eventId_ < eventId)
          Left(Problem(
            s"Invalid AgentEventsObserved(${EventId.toString(eventId)}) event; expected eventId >= ${EventId.toString(eventId)}"))
        else
          Right(copy(eventId = eventId_))
    }
}

object AgentRefState
{
  implicit val jsonCodec = deriveCodec[AgentRefState]

  def apply(agentRef: AgentRef) =
    new AgentRefState(agentRef, None, EventId.BeforeFirst)
}
