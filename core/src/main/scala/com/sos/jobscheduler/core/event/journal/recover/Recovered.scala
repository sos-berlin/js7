package com.sos.jobscheduler.core.event.journal.recover

import akka.actor.{ActorRef, ActorRefFactory}
import com.sos.jobscheduler.common.event.PositionAnd
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta, RecoveredJournalingActors}
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.core.event.state.JournalStateBuilder
import com.sos.jobscheduler.data.event.{Event, EventId, JournalId, JournaledState}
import com.typesafe.config.Config
import java.nio.file.Path
import scala.language.higherKinds

final case class Recovered[S <: JournaledState[S, E], E <: Event](
  journalMeta: JournalMeta,
  /** The recovered state's EventId. */
  eventId: EventId,
  /** The recovered jounaled file's length and path. */
  positionAndFile: Option[PositionAnd[Path]],
  /** The recovered journal file's position before the events. */
  firstEventPosition: Option[Long],
  /** The recovered journal file's JournalHeader. */
  fileJournalHeader: Option[JournalHeader],
  /** The calculated recovered JournalHeader to continue with. */
  recoveredJournalHeader: Option[JournalHeader],
  /** The recovered state */
  maybeState: Option[S],
  newStateBuilder: () => JournalStateBuilder[S, E],
  eventWatch: JournalEventWatch,
  config: Config)
extends AutoCloseable
{
  def close() =
    eventWatch.close()

  def journalId: Option[JournalId] = recoveredJournalHeader.map(_.journalId)

  // Suppresses Config (which may contain secrets)
  override def toString = s"Recovered($journalMeta,$eventId,$positionAndFile," +
    s"$fileJournalHeader,$recoveredJournalHeader,$maybeState,$newStateBuilder,$eventWatch,Config)"

  def startJournalAndFinishRecovery(
    journalActor: ActorRef,
    recoveredActors: RecoveredJournalingActors = RecoveredJournalingActors.Empty,
    requireClusterAcknowledgement: Boolean = false)
    (implicit actorRefFactory: ActorRefFactory)
  =
    JournalRecoverer.startJournalAndFinishRecovery[Event](journalActor, recoveredActors,
      requireClusterAcknowledgement = requireClusterAcknowledgement,
      Some(eventWatch),
      recoveredJournalHeader.map(_.journalId), recoveredJournalHeader)
}
