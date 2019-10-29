package com.sos.jobscheduler.core.event.journal.recover

import akka.actor.{ActorRef, ActorRefFactory}
import com.sos.jobscheduler.common.event.PositionAnd
import com.sos.jobscheduler.common.scalautil.Closer.ops._
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta, RecoveredJournalingActors}
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.core.event.state.JournalStateBuilder
import com.sos.jobscheduler.data.event.{Event, JournaledState}
import com.typesafe.config.Config
import java.nio.file.Path
import scala.language.higherKinds

final class Recovered[S <: JournaledState[S, E], E <: Event](
  val journalMeta: JournalMeta,
  val journalStateBuilder: JournalStateBuilder[S, E],
  val config: Config,
  val positionAndFile: Option[PositionAnd[Path]],
  val journalHeader: Option[JournalHeader],
  val maybeState: Option[S])
extends HasCloser
{
  lazy val eventWatch = new JournalEventWatch(journalMeta, config)
    .closeWithCloser

  def startJournalAndFinishRecovery(
    journalActor: ActorRef,
    recoveredActors: RecoveredJournalingActors = RecoveredJournalingActors.Empty)
    (implicit actorRefFactory: ActorRefFactory)
  =
    JournalRecoverer.startJournalAndFinishRecovery[Event](journalActor, recoveredActors, Some(eventWatch),
      journalHeader.map(_.journalId), journalHeader)

  def eventId = journalStateBuilder.eventId

  def totalRunningTime = journalStateBuilder.totalRunningTime
}
