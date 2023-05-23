package js7.journal.recover

import cats.syntax.option.*
import com.typesafe.config.Config
import js7.data.cluster.ClusterState
import js7.data.cluster.ClusterState.FailedOver
import js7.data.event.{EventId, JournalId, SnapshotableState}
import js7.data.node.NodeId
import js7.journal.data.JournalLocation
import js7.journal.watch.JournalEventWatch
import scala.concurrent.duration.*

final class Recovered[S <: SnapshotableState[S]] private(
  val journalLocation: JournalLocation,
  val recoveredJournalFile: Option[RecoveredJournalFile[S]],
  val totalRunningSince: Deadline,
  config: Config,
  val eventWatch: JournalEventWatch)
  (implicit S: SnapshotableState.Companion[S])
extends AutoCloseable
{
  def close() =
    eventWatch.close()

  /** Replace this Recovered.
    * The caller must not close the old one
    * because JournalEventWatch remains the same.
    */
  def changeRecoveredJournalFile(recoveredJournalFile: Option[RecoveredJournalFile[S]]) =
    new Recovered(journalLocation, recoveredJournalFile, totalRunningSince, config, eventWatch)

  def eventId: EventId =
    recoveredJournalFile.fold(EventId.BeforeFirst)(_.eventId)

  def clusterState: ClusterState =
    state.clusterState

  def failedNodeId: Option[NodeId] =
    clusterState.some.collect {
      case o: FailedOver => o.passiveId
    }

  def state: S =
    recoveredJournalFile.fold(S.empty)(_.state)

  def recoveredState: Option[S] =
    recoveredJournalFile.map(_.state)

  def journalId: Option[JournalId] =
    recoveredJournalFile.map(_.journalId)

  def extract: Recovered.Extract =
    Recovered.Extract(eventId, eventWatch, totalRunningSince)

  // Suppresses Config (which may contain secrets)
  override def toString = s"Recovered($journalLocation,$recoveredJournalFile,$eventWatch)"
}

object Recovered
{
  def fromJournalFile[S <: SnapshotableState[S]](
    journalLocation: JournalLocation,
    recoveredJournalFile: RecoveredJournalFile[S],
    totalRunningSince: Deadline,
    config: Config)
    (implicit S: SnapshotableState.Companion[S])
  : Recovered[S] =
    new Recovered(
      journalLocation,
      Some(recoveredJournalFile),
      totalRunningSince,
      config,
      new JournalEventWatch(journalLocation, config, Some(recoveredJournalFile.eventId)))

  def noJournalFile[S <: SnapshotableState[S]](
    journalLocation: JournalLocation,
    totalRunningSince: Deadline,
    config: Config)
    (implicit S: SnapshotableState.Companion[S])
  : Recovered[S] =
    new Recovered(
      journalLocation,
      recoveredJournalFile = None,
      totalRunningSince,
      config,
      new JournalEventWatch(journalLocation, config, Some(EventId.BeforeFirst)))

  final case class Extract(
    eventId: EventId,
    eventWatch: JournalEventWatch,
    totalRunningSince: Deadline)
  {
    override def productPrefix = "Recovered.Extract"
  }
}
