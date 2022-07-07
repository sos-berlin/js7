package js7.data.agent

import io.circe.generic.semiauto.deriveCodec
import js7.base.problem.{Checked, Problem}
import js7.base.version.Version
import js7.data.agent.AgentRefStateEvent.{AgentCoupled, AgentCouplingFailed, AgentDedicated, AgentEventsObserved, AgentReady, AgentReset, AgentResetStarted, AgentShutDown}
import js7.data.delegate.DelegateCouplingState
import js7.data.delegate.DelegateCouplingState.{Coupled, Reset, Resetting, ShutDown}
import js7.data.event.EventId
import js7.data.item.UnsignedSimpleItemState

final case class AgentRefState(
  agentRef: AgentRef,
  agentRunId: Option[AgentRunId],
  timezone: Option[String],
  version: Option[Version],
  couplingState: DelegateCouplingState,
  eventId: EventId,
  problem: Option[Problem])
extends UnsignedSimpleItemState
{
  protected type Self = AgentRefState
  val companion = AgentRefState

  val item = agentRef

  def updateItem(item: AgentRef): Checked[AgentRefState] =
    Right(copy(agentRef = item))

  def agentPath = agentRef.path

  def agentPathToAttachedState = Map.empty

  def applyEvent(event: AgentRefStateEvent): Checked[AgentRefState] =
    event match {
      case AgentDedicated(agentRunId_, eventId_) =>
        if (agentRunId.isDefined || eventId != EventId.BeforeFirst)
          Left(Problem("Duplicate AgentDedicated event: " + event))
        else
          Right(copy(
            agentRunId = Some(agentRunId_),
            eventId = eventId_.getOrElse(EventId.BeforeFirst),
            problem = None))

      case AgentReady(version, timezone) =>
        Right(copy(
          couplingState = Coupled,
          version = version,
          timezone = Some(timezone),
          problem = None))

      case AgentShutDown =>
        Right(copy(
          couplingState = ShutDown,
          problem = None))

      case AgentCoupled =>
        couplingState match {
          case Resetting(_) =>
            // Required until ControllerOrderKeeper ResetAgent uses persistence.lock !!!
            scribe.debug("(WARN) Ignoring AgentCoupled event due to Resetting state")
            Right(this)

          case _ =>
            Right(copy(
              couplingState = Coupled,
              problem = None))
        }

      case AgentCouplingFailed(problem) =>
        Right(copy(
          problem = Some(problem)))

      case AgentEventsObserved(eventId_) =>
        if (eventId_ < eventId)
          Left(Problem(
            s"Invalid AgentEventsObserved(${EventId.toString(eventId_)}) event;" +
              s" expected eventId >= ${EventId.toString(eventId)}"))
        else
          Right(copy(
            eventId = eventId_))

      case AgentResetStarted(force) =>
        if (agentRunId.isEmpty && !force)
          Left(Problem.pure("Agent is already marked as 'Reset' in Controller's AgentRef"))
        else
          Right(copy(
            couplingState = Resetting(force),
            agentRunId = if (force) None else agentRunId,
            eventId = EventId.BeforeFirst,
            timezone = None,
            problem = None))

      case AgentReset =>
        Right(copy(
          couplingState = Reset.byCommand,
          agentRunId = None,
          problem = None))
    }
}

object AgentRefState extends UnsignedSimpleItemState.Companion[AgentRefState]
{
  type Path = AgentPath
  type ItemState = AgentRefState
  type Item = AgentRef

  implicit val jsonCodec = deriveCodec[AgentRefState]

  def apply(agentRef: AgentRef) =
    new AgentRefState(agentRef, None, None, None, Reset.fresh, EventId.BeforeFirst, None)
}
