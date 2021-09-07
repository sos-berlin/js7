package js7.data.agent

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.{Checked, Problem}
import js7.data.agent.AgentRefState.{Coupled, CouplingFailed, CouplingState, _}
import js7.data.agent.AgentRefStateEvent.{AgentCouplingFailed, AgentCreated, AgentEventsObserved, AgentReady, AgentReset, AgentResetStarted, AgentShutDown}
import js7.data.event.EventId
import js7.data.item.UnsignedSimpleItemState

final case class AgentRefState(
  agentRef: AgentRef,
  agentRunId: Option[AgentRunId],
  timezone: Option[String],
  couplingState: CouplingState,
  eventId: EventId)
extends UnsignedSimpleItemState
{
  def item = agentRef

  def agentPath = agentRef.path

  def agentPathToAttachedState = Map.empty

  def applyEvent(event: AgentRefStateEvent): Checked[AgentRefState] =
    event match {
      case AgentCreated(agentRunId_, eventId_) =>
        if (agentRunId.isDefined || eventId != EventId.BeforeFirst)
          Left(Problem("Duplicate AgentCreated event: " + event))
        else
          Right(copy(
            agentRunId = Some(agentRunId_),
            eventId = eventId_.getOrElse(EventId.BeforeFirst)))

      case AgentReady(timezone) =>
        Right(copy(
          couplingState = Coupled,
          timezone = Some(timezone)))

      case AgentShutDown =>
        Right(copy(
          couplingState = ShutDown))

      case AgentCouplingFailed(problem) =>
        Right(copy(
          couplingState = CouplingFailed(problem)))

      case AgentEventsObserved(eventId_) =>
        if (eventId_ < eventId)
          Left(Problem(
            s"Invalid AgentEventsObserved(${EventId.toString(eventId_)}) event;" +
              s" expected eventId >= ${EventId.toString(eventId)}"))
        else
          Right(copy(
            eventId = eventId_))

      case AgentResetStarted =>
        Right(copy(
          couplingState = Resetting,
          agentRunId = None,
          eventId = EventId.BeforeFirst,
          timezone = None))

      case AgentReset =>
        Right(copy(
          couplingState = Reset))
    }
}

object AgentRefState
{
  implicit val jsonCodec = deriveCodec[AgentRefState]

  def apply(agentRef: AgentRef) =
    new AgentRefState(agentRef, None, None, Reset, EventId.BeforeFirst)

  sealed trait CouplingState
  case object Reset extends CouplingState
  case object Coupled extends CouplingState
  case class CouplingFailed(problem: Problem) extends CouplingState
  case object ShutDown extends CouplingState
  case object Resetting extends CouplingState

  object CouplingState {
    implicit val jsonCodec: TypedJsonCodec[CouplingState] = TypedJsonCodec(
      Subtype(Reset),
      Subtype(Coupled),
      Subtype(deriveCodec[CouplingFailed]),
      Subtype(ShutDown),
      Subtype(Resetting))
  }
}
