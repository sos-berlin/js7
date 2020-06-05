package js7.agent.scheduler

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils._
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, EventId, JournalState, JournaledState, KeyedEvent}
import js7.data.master.MasterId
import monix.reactive.Observable

final case class AgentServerState(
  standards: JournaledState.Standards,
  idToMaster: Map[MasterId, RegisteredMaster])
extends JournaledState[AgentServerState]
{
  def withEventId(eventId: EventId) = this  // ???

  def withStandards(standards: JournaledState.Standards) =
    copy(standards = standards)

  def toSnapshotObservable =
    journalState.toSnapshotObservable ++
      Observable.fromIterable(idToMaster.values)

  def applySnapshot(snapshot: Any): Checked[AgentServerState] =
    snapshot match {
      case o: JournalState =>
        Right(copy(
          standards = standards.copy(
            journalState = o)))

      case o: RegisteredMaster =>
        if (idToMaster contains o.masterId)
          Left(Problem.pure(s"Duplicate snapshot for register Master: $o"))
        else
          Right(copy(idToMaster = idToMaster + (o.masterId -> o)))

      case o =>
        Left(Problem.pure(s"Unknown snapshot for AgentServer: ${o.getClass.scalaName}"))
    }

  def applyEvent(keyedEvent: KeyedEvent[Event]) =
    keyedEvent match {
      case KeyedEvent(_: NoKey, AgentEvent.MasterRegistered(masterId, agentRefPath, agentRunId)) =>
        if (idToMaster contains masterId)
          Left(Problem.pure(s"Duplicate event for register Master: $keyedEvent"))
        else
          Right(copy(
            idToMaster = idToMaster + (masterId -> RegisteredMaster(masterId, agentRefPath, agentRunId))))

      case keyedEvent =>
        applyStandardEvent(keyedEvent)
    }
}

object AgentServerState
{
  val empty = AgentServerState(JournaledState.Standards.empty, Map.empty)
}
