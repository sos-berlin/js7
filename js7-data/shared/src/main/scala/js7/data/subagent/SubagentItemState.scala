package js7.data.subagent

import io.circe.generic.semiauto.deriveCodec
import js7.base.problem.{Checked, Problem}
import js7.data.delegate.DelegateCouplingState
import js7.data.delegate.DelegateCouplingState.{Coupled, Reset, ShutDown}
import js7.data.event.EventId
import js7.data.item.UnsignedSimpleItemState
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentCouplingFailed, SubagentDedicated, SubagentEventsObserved, SubagentRestarted, SubagentShutdown}

final case class SubagentItemState(
  subagentItem: SubagentItem,
  subagentRunId: Option[SubagentRunId],
  couplingState: DelegateCouplingState,
  eventId: EventId,
  problem: Option[Problem])
extends UnsignedSimpleItemState
{
  protected type Item = SubagentItem

  def item = subagentItem

  def subagentId = subagentItem.id

  def pathRev = item.pathRev

  def isCoupled =
    couplingState == Coupled && problem.isEmpty

  def applyEvent(event: SubagentItemStateEvent): Checked[SubagentItemState] =
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
            s"Duplicate SubagentDedicated event: " + this.subagentRunId + " -> " + pathRev))
        else
          Right(copy(
            couplingState = Coupled,
            subagentRunId = Some(subagentRunId),
            problem = None))

      case SubagentCoupled =>
        Right(copy(
          couplingState = Coupled,
          problem = None))

      case SubagentRestarted =>
        Right(copy(
          couplingState = Reset,
          subagentRunId = None,
          eventId = EventId.BeforeFirst))

      case SubagentShutdown =>
        Right(copy(
          couplingState = ShutDown,
          subagentRunId = None,
          eventId = EventId.BeforeFirst))
    }
}

object SubagentItemState
{
  def initial(subagentItem: SubagentItem) =
    SubagentItemState(subagentItem, None, DelegateCouplingState.Reset, eventId = EventId.BeforeFirst,
      None)

  implicit val jsonCodec = deriveCodec[SubagentItemState]
}
