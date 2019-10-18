package com.sos.jobscheduler.core.event.state

import akka.actor.{ActorRef, ActorRefFactory}
import com.sos.jobscheduler.common.event.PositionAnd
import com.sos.jobscheduler.common.scalautil.Closer.ops.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.data.event.{Event, JournalId, JournaledState}
import com.typesafe.config.Config
import java.nio.file.Path
import scala.concurrent.duration.FiniteDuration
import shapeless.tag.@@

final class Recovered[S <: JournaledState[S, E], E <: Event](recoverer: JournaledStateRecoverer[S, E], config: Config)
extends HasCloser
{
  lazy val eventWatch = new JournalEventWatch(recoverer.journalMeta, config)
    .closeWithCloser

  def journalMeta: JournalMeta =
    recoverer.journalMeta

  def maybeState: Option[S] =
    recoverer.state

  def totalRunningTime: FiniteDuration =
    recoverer.totalRunningTime

  def journalId: Option[JournalId] =
    recoverer.journalId

  def eventId = recoverer.lastRecoveredEventId

  def positionAndFile: Option[PositionAnd[Path]] =
    recoverer.positionAndFile

  //def startJournal(journalActor: ActorRef @@ JournalActor.type): Task[Completed] =
  //  recoverer.startJournal(journalActor = journalActor, journalingObserver = Some(eventWatch))

  def startJournalAndFinishRecovery(journalActor: ActorRef @@ JournalActor.type)(implicit arf: ActorRefFactory): Unit =
    recoverer.startJournalAndFinishRecovery(journalActor = journalActor, journalingObserver = Some(eventWatch))
}
