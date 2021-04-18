package js7.agent.scheduler

import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, EventId, JournalEvent, JournalState, JournaledState, KeyedEvent, KeyedEventTypedJsonCodec}
import monix.reactive.Observable

final case class AgentServerState(
  eventId: EventId,
  standards: JournaledState.Standards,
  idToController: Map[ControllerId, RegisteredController])
extends JournaledState[AgentServerState]
{
  def withEventId(eventId: EventId) =
    copy(eventId = eventId)

  def withStandards(standards: JournaledState.Standards) =
    copy(standards = standards)

  def estimatedSnapshotSize =
    standards.snapshotSize + idToController.size

  def toSnapshotObservable =
    standards.toSnapshotObservable ++
      Observable.fromIterable(idToController.values)

  def applySnapshot(snapshot: Any): Checked[AgentServerState] =
    snapshot match {
      case o: JournalState =>
        Right(copy(
          standards = standards.copy(
            journalState = o)))

      case o: RegisteredController =>
        if (idToController contains o.controllerId)
          Left(Problem.pure(s"Duplicate snapshot for register Controller: $o"))
        else
          Right(copy(idToController = idToController + (o.controllerId -> o)))

      case o =>
        Left(Problem.pure(s"Unknown snapshot for AgentServer: ${o.getClass.scalaName}"))
    }

  def applyEvent(keyedEvent: KeyedEvent[Event]) =
    keyedEvent match {
      case KeyedEvent(_: NoKey, AgentServerEvent.ControllerRegistered(controllerId, agentId, agentRunId)) =>
        if (idToController contains controllerId)
          Left(Problem.pure(s"Duplicate event for register Controller: $keyedEvent"))
        else
          Right(copy(
            idToController = idToController + (controllerId -> RegisteredController(controllerId, agentId, agentRunId))))

      case keyedEvent =>
        applyStandardEvent(keyedEvent)
    }
}

object AgentServerState extends JournaledState.Companion[AgentServerState]
{
  val empty = AgentServerState(EventId.BeforeFirst, JournaledState.Standards.empty, Map.empty)

  implicit val snapshotObjectJsonCodec: TypedJsonCodec[Any] =
    TypedJsonCodec[Any](
      Subtype[JournalState],
      Subtype[RegisteredController])

  implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec[Event](
      KeyedSubtype[JournalEvent],
      KeyedSubtype[AgentServerEvent])

  def newBuilder() = new AgentServerStateBuilder

  protected def InventoryItems = Nil
}
