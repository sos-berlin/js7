package js7.data.subagent

import io.circe.generic.semiauto.deriveCodec
import js7.base.problem.{Checked, Problem}
import js7.data.agent.DelegateCouplingState
import js7.data.agent.DelegateCouplingState.Coupled
import js7.data.event.EventId
import js7.data.item.UnsignedSimpleItemState
import js7.data.subagent.SubagentRefStateEvent.{SubagentCoupled, SubagentCouplingFailed, SubagentDedicated, SubagentEventsObserved}

final case class SubagentRefState(
  subagentRef: SubagentRef,
  subagentRunId: Option[SubagentRunId],
  couplingState: DelegateCouplingState,
  eventId: EventId,
  problem: Option[Problem])
extends UnsignedSimpleItemState
{
  protected type Item = SubagentRef

  def item = subagentRef

  def subagentId = subagentRef.id

  def applyEvent(event: SubagentRefStateEvent): Checked[SubagentRefState] =
    event match {
      case SubagentEventsObserved(untilEventId) =>
        Right(copy(
          eventId = untilEventId))

      case SubagentCouplingFailed(problem) =>
        Right(copy(
          problem = Some(problem)))

      case SubagentDedicated(subagentRunId) =>
        if (this.subagentRunId.exists(_ != subagentRunId))
          Left(Problem.pure(
            s"Duplicate SubagentDedicated event: " + this.subagentRunId + " -> " + subagentId))
        else
          Right(copy(
            couplingState = Coupled,
            subagentRunId = Some(subagentRunId),
            problem = None))

      case SubagentCoupled =>
        Right(copy(
          couplingState = Coupled,
          problem = None))
    }
}

object SubagentRefState
{
  def initial(subagentRef: SubagentRef) =
    SubagentRefState(subagentRef, None, DelegateCouplingState.Reset, eventId = EventId.BeforeFirst,
      None)

  implicit val jsonCodec = deriveCodec[SubagentRefState]
}
