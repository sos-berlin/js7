package js7.data.agent

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.{Checked, Problem}
import js7.data.agent.AgentRefState.{Coupled, CouplingFailed, CouplingState}
import js7.data.agent.AgentRefStateEvent.{AgentCouplingFailed, AgentEventsObserved, AgentReady, AgentRegisteredController}
import js7.data.event.EventId
import js7.data.item.SimpleItemState

final case class AgentRefState(
  agentRef: AgentRef,
  agentRunId: Option[AgentRunId],
  timezone: Option[String],
  couplingState: CouplingState,
  eventId: EventId)
extends SimpleItemState
{
  def item = agentRef

  def agentId = agentRef.id

  def applyEvent(event: AgentRefStateEvent): Checked[AgentRefState] =
    event match {
      case AgentRegisteredController(agentRunId_) =>
        if (agentRunId.isDefined || eventId != EventId.BeforeFirst)
          Left(Problem("Duplicate AgentRegisteredController event: " + event))
        else
          Right(copy(
            agentRunId = Some(agentRunId_)))

      case AgentReady(timezone) =>
        Right(copy(
          couplingState = Coupled,
          timezone = Some(timezone)))

      case AgentCouplingFailed(problem) =>
        Right(copy(couplingState = CouplingFailed(problem)))

      case AgentEventsObserved(eventId_) =>
        if (eventId_ < eventId)
          Left(Problem(
            s"Invalid AgentEventsObserved(${EventId.toString(eventId_)}) event; expected eventId >= ${EventId.toString(eventId)}"))
        else
          Right(copy(eventId = eventId_))
    }
}

object AgentRefState
{
  implicit val jsonCodec = deriveCodec[AgentRefState]

  def apply(agentRef: AgentRef) =
    new AgentRefState(agentRef, None, None, Decoupled, EventId.BeforeFirst)

  sealed trait CouplingState
  case object Coupled extends CouplingState
  case class CouplingFailed(problem: Problem) extends CouplingState
  case object Decoupled extends CouplingState

  object CouplingState {
    implicit val jsonCodec: TypedJsonCodec[CouplingState] = TypedJsonCodec(
      Subtype(Coupled),
      Subtype(deriveCodec[CouplingFailed]),
      Subtype(Decoupled))
  }
}
