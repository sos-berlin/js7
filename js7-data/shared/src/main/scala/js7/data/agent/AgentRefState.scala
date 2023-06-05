package js7.data.agent

import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Decoder, Encoder}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.data.agent.AgentRefStateEvent.{AgentCoupled, AgentCouplingFailed, AgentDedicated, AgentEventsObserved, AgentMirroredEvent, AgentReady, AgentReset, AgentResetStarted, AgentShutDown}
import js7.data.cluster.{ClusterEvent, ClusterState}
import js7.data.delegate.DelegateCouplingState
import js7.data.delegate.DelegateCouplingState.{Coupled, Reset, Resetting, ShutDown}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, KeyedEvent}
import js7.data.item.UnsignedSimpleItemState
import js7.data.platform.PlatformInfo

final case class AgentRefState(
  agentRef: AgentRef,
  agentRunId: Option[AgentRunId],
  timezone: Option[String],
  couplingState: DelegateCouplingState,
  eventId: EventId,
  problem: Option[Problem],
  clusterState: ClusterState,
  platformInfo: Option[PlatformInfo])
extends UnsignedSimpleItemState
{
  protected type Self = AgentRefState
  val companion: AgentRefState.type = AgentRefState

  val item: AgentRef = agentRef
  def path: AgentPath = item.path

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

      case AgentReady(timezone, platformInfo) =>
        Right(copy(
          couplingState = Coupled,
          timezone = Some(timezone),
          problem = None,
          platformInfo = platformInfo))

      case AgentShutDown =>
        Right(copy(
          couplingState = ShutDown,
          problem = None))

      case AgentCoupled =>
        couplingState match {
          case Resetting(_) =>
            // Required until ControllerOrderKeeper ResetAgent uses journal.lock !!!
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
            problem = None,
            clusterState = ClusterState.Empty))

      case AgentReset =>
        Right(copy(
          couplingState = Reset.byCommand,
          agentRunId = None,
          problem = None,
          platformInfo = None))

      case AgentMirroredEvent(keyedEvent) =>
        keyedEvent match {
          case KeyedEvent(_: NoKey, event: ClusterEvent) =>
            clusterState.applyEvent(event)
              .map(clusterState => copy(clusterState = clusterState))
              .left.map(_.withPrefix(s"$agentPath <-: AgentMirroredEvent:"))

          case _ => Left(Problem(
            s"Unknown mirrored Event in AgentMirroredEvent: ${keyedEvent.getClass.shortClassName}"))
        }
    }
}

object AgentRefState extends UnsignedSimpleItemState.Companion[AgentRefState]
{
  type Key = AgentPath
  type Item = AgentRef
  override type ItemState = AgentRefState

  def apply(agentRef: AgentRef) =
    new AgentRefState(agentRef, None, None, Reset.fresh, EventId.BeforeFirst, None,
      ClusterState.Empty, None)

  implicit val jsonEncoder: Encoder.AsObject[AgentRefState] =
    deriveEncoder

  implicit val jsonDecoder: Decoder[AgentRefState] =
    c => for {
      agentRef <- c.get[AgentRef]("agentRef")
      agentRunId <- c.get[Option[AgentRunId]]("agentRunId")
      timezone <- c.get[Option[String]]("timezone")
      couplingState <- c.get[DelegateCouplingState]("couplingState")
      eventId <- c.get[EventId]("eventId")
      problem <- c.get[Option[Problem]]("problem")
      clusterState <- c.getOrElse[ClusterState]("clusterState")(ClusterState.Empty)
      platformInfo <- c.get[Option[PlatformInfo]]("platformInfo")
    } yield
      AgentRefState(
        agentRef, agentRunId, timezone, couplingState, eventId, problem, clusterState, platformInfo)
}
