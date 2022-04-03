package js7.data.subagent

import io.circe.generic.extras.Configuration.default.withDefaults
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Encoder, JsonObject}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.delegate.DelegateCouplingState
import js7.data.delegate.DelegateCouplingState.{Coupled, Reset, ShutDown}
import js7.data.event.EventId
import js7.data.item.UnsignedSimpleItemState
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentCouplingFailed, SubagentDedicated, SubagentEventsObserved, SubagentRestarted, SubagentShutdown}

final case class SubagentItemState(
  subagentItem: SubagentItem,
  subagentRunId: Option[SubagentRunId],
  couplingState: DelegateCouplingState,
  isDetaching: Boolean = false,  // Agent only
  eventId: EventId,
  problem: Option[Problem])
extends UnsignedSimpleItemState
{
  protected type Item = SubagentItem

  def item = subagentItem

  def subagentId = subagentItem.id

  def pathRev = item.pathRev

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
    SubagentItemState(subagentItem, None, DelegateCouplingState.Reset,
      isDetaching = false,
      eventId = EventId.BeforeFirst,
      None)

  private val jsonDecoder = {
    implicit val x = withDefaults
    deriveConfiguredDecoder[SubagentItemState]
  }

  private val jsonEncoder: Encoder.AsObject[SubagentItemState] =
    o => JsonObject(
      "subagentItem" -> o.subagentItem.asJson,
      "subagentRunId" -> o.subagentRunId.asJson,
      "couplingState" -> o.couplingState.asJson,
      "isDetaching" -> o.isDetaching.?.asJson,
      "eventId" -> o.eventId.asJson,
      "problem" -> o.problem.asJson)

  implicit val jsonCodec = Codec.AsObject.from(jsonDecoder, jsonEncoder)
}
