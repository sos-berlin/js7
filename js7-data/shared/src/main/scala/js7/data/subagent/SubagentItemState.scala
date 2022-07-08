package js7.data.subagent

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.base.version.Version
import js7.data.delegate.DelegateCouplingState
import js7.data.delegate.DelegateCouplingState.{Coupled, Reset, Resetting}
import js7.data.event.EventId
import js7.data.item.UnsignedSimpleItemState
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentCouplingFailed, SubagentDedicated, SubagentEventsObserved, SubagentReset, SubagentResetStarted, SubagentResetStartedByController, SubagentRestarted, SubagentShutdown}

final case class SubagentItemState(
  subagentItem: SubagentItem,
  subagentRunId: Option[SubagentRunId],
  version: Option[Version],
  couplingState: DelegateCouplingState,
  isDetaching: Boolean = false,  // Agent only
  isResettingForcibly: Option[Boolean] = None,  // Controller only
  eventId: EventId,
  problem: Option[Problem] = None)
extends UnsignedSimpleItemState
{
  protected type Self = SubagentItemState
  val companion = SubagentItemState

  val item = subagentItem

  def updateItem(item: SubagentItem) =
    for (_ <- item.agentPath == subagentItem.agentPath !! Problem.pure(
      "A Subagent's AgentPath cannot be changed"))
    yield copy(subagentItem = item)

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

      case SubagentDedicated(subagentRunId, version) =>
        if (this.subagentRunId.exists(_ != subagentRunId))
          Left(Problem.pure("Duplicate SubagentDedicated event: " + (item.id <-: event) +
            ", this.subagentRunId=" + this.subagentRunId))
        else
          Right(copy(
            couplingState = Coupled,
            subagentRunId = Some(subagentRunId),
            version = version,
            problem = None))

      case SubagentCoupled =>
        Right(copy(
          couplingState = Coupled,
          problem = None))

      case SubagentRestarted =>
        Right(copy(
          couplingState = Reset.restart,
          subagentRunId = None,
          eventId = EventId.BeforeFirst,
          problem = None))

      case SubagentShutdown =>
        Right(copy(
          couplingState = Reset.shutdown,
          subagentRunId = None,
          eventId = EventId.BeforeFirst,
          problem = None))

      case SubagentResetStartedByController(force) =>
        Right(copy(
          // Do not touch anything else!
          // Only the Agent Director is allowed to to update other fields then isResettingForcibly
          isResettingForcibly = Some(force)))

      case SubagentResetStarted(force) =>
        Right(copy(
          couplingState = Resetting(force = force),
          problem = None))

      case SubagentReset =>
        Right(copy(
          couplingState = Reset.byCommand,
          isResettingForcibly = None,
          subagentRunId = None,
          eventId = EventId.BeforeFirst,
          problem = None))
    }
}

object SubagentItemState extends UnsignedSimpleItemState.Companion[SubagentItemState]
{
  type Path = SubagentId
  type ItemState = SubagentItemState
  type Item = SubagentItem

  def initial(subagentItem: SubagentItem) =
    SubagentItemState(subagentItem, None, None, DelegateCouplingState.Reset.fresh,
      eventId = EventId.BeforeFirst)

  implicit val jsonDecoder: Decoder[SubagentItemState] =
   c => for {
     subagentItem <- c.get[SubagentItem]("subagentItem") orElse c.get[SubagentItem]("subagentRef")
     subagentRunId <- c.get[Option[SubagentRunId]]("subagentRunId")
     version <- c.get[Option[Version]]("version")
     couplingState <- c.get[DelegateCouplingState]("couplingState")
     isDetaching <- c.getOrElse[Boolean]("isDetaching")(false)
     isResettingForcibly <- c.get[Option[Boolean]]("isResettingForcibly")
     eventId <- c.get[EventId]("eventId")
     problem <- c.get[Option[Problem]]("problem")
   } yield SubagentItemState(
     subagentItem, subagentRunId, version, couplingState,
     isDetaching = isDetaching, isResettingForcibly = isResettingForcibly,
     eventId = eventId,
     problem)

  implicit val jsonEncoder: Encoder.AsObject[SubagentItemState] =
    o => JsonObject(
      "subagentItem" -> o.subagentItem.asJson,
      "subagentRunId" -> o.subagentRunId.asJson,
      "version" -> o.version.asJson,
      "couplingState" -> o.couplingState.asJson,
      "isDetaching" -> o.isDetaching.?.asJson,
      "isResettingForcibly" -> o.isResettingForcibly.asJson,
      "eventId" -> o.eventId.asJson,
      "problem" -> o.problem.asJson)
}
