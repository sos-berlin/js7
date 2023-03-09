package js7.journal.recover

import com.typesafe.config.Config
import js7.data.cluster.ClusterState
import js7.data.event.{EventId, JournalId, SnapshotableState}
import js7.journal.data.JournalMeta
import js7.journal.watch.JournalEventWatch
import scala.concurrent.duration.*

final class Recovered[S <: SnapshotableState[S]] private(
  val journalMeta: JournalMeta,
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
    new Recovered(journalMeta, recoveredJournalFile, totalRunningSince, config, eventWatch)

  def eventId: EventId =
    recoveredJournalFile.fold(EventId.BeforeFirst)(_.eventId)

  def clusterState: ClusterState =
    state.clusterState

  def state: S =
    recoveredJournalFile.fold(S.empty)(_.state)

  def recoveredState: Option[S] =
    recoveredJournalFile.map(_.state)

  def journalId: Option[JournalId] =
    recoveredJournalFile.map(_.journalId)

  def extract: Recovered.Extract =
    Recovered.Extract(eventId, eventWatch, totalRunningSince)

  // Suppresses Config (which may contain secrets)
  override def toString = s"Recovered($journalMeta,$recoveredJournalFile,$eventWatch)"
}

object Recovered
{
  def fromJournalFile[S <: SnapshotableState[S]](
    journalMeta: JournalMeta,
    recoveredJournalFile: RecoveredJournalFile[S],
    totalRunningSince: Deadline,
    config: Config)
    (implicit S: SnapshotableState.Companion[S])
  : Recovered[S] =
    new Recovered(
      journalMeta,
      Some(recoveredJournalFile),
      totalRunningSince,
      config,
      new JournalEventWatch(journalMeta, config, Some(recoveredJournalFile.eventId)))

  def noJournalFile[S <: SnapshotableState[S]](
    journalMeta: JournalMeta,
    totalRunningSince: Deadline,
    config: Config)
    (implicit S: SnapshotableState.Companion[S])
  : Recovered[S] =
    new Recovered(
      journalMeta,
      recoveredJournalFile = None,
      totalRunningSince,
      config,
      new JournalEventWatch(journalMeta, config, Some(EventId.BeforeFirst)))

  final case class Extract(
    eventId: EventId,
    eventWatch: JournalEventWatch,
    totalRunningSince: Deadline)
  {
    override def productPrefix = "Recovered.Extract"
  }
}
