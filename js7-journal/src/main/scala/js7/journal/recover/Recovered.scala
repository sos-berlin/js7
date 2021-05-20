package js7.journal.recover

import com.typesafe.config.Config
import js7.base.utils.SetOnce
import js7.data.cluster.ClusterState
import js7.data.event.{EventId, JournalId, JournaledState}
import js7.journal.data.JournalMeta
import js7.journal.watch.JournalEventWatch
import scala.concurrent.duration._

final class Recovered[S <: JournaledState[S]] private(
  journalMeta: JournalMeta,
  val recoveredJournalFile: Option[RecoveredJournalFile[S]],
  val totalRunningSince: Deadline,
  config: Config,
  val eventWatch: JournalEventWatch,
  journalId_ : Option[JournalId])
  (implicit S: JournaledState.Companion[S])
extends AutoCloseable
{
  private val journalIdOnce = SetOnce.fromOption(journalId_)

  def close() =
    eventWatch.close()

  /** Replace this Recovered.
    * The caller must not close the old one
    * because JournalEventWatch remains the same.
    */
  def changeRecoveredJournalFile(recoveredJournalFile: Option[RecoveredJournalFile[S]]) =
    new Recovered(journalMeta, recoveredJournalFile, totalRunningSince, config, eventWatch, journalIdOnce.toOption)

  def maybeJournalId = recoveredJournalFile.map(_.journalId)

  def eventId: EventId =
    recoveredJournalFile.fold(EventId.BeforeFirst)(_.eventId)

  def clusterState: ClusterState =
    state.clusterState

  def state: S =
    recoveredJournalFile.fold(S.empty)(_.state)

  def recoveredState: Option[S] =
    recoveredJournalFile.map(_.state)

  def journalId: Option[JournalId] =
    journalIdOnce.toOption

  // Suppresses Config (which may contain secrets)
  override def toString = s"Recovered($journalMeta,$recoveredJournalFile,$eventWatch,Config)"

  def onJournalIdReplicated(journalId: JournalId): Unit =
    journalIdOnce := journalId
}

object Recovered
{
  def apply[S <: JournaledState[S]](
    journalMeta: JournalMeta,
    recoveredJournalFile: Option[RecoveredJournalFile[S]],
    totalRunningSince: Deadline,
    config: Config)
    (implicit S: JournaledState.Companion[S])
  : Recovered[S] = {
    val recoveredEventId = recoveredJournalFile.fold(EventId.BeforeFirst)(_.eventId)
    new Recovered(
      journalMeta, recoveredJournalFile, totalRunningSince, config,
      new JournalEventWatch(journalMeta, config, Some(recoveredEventId)),
      recoveredJournalFile.map(_.journalId))
  }
}
