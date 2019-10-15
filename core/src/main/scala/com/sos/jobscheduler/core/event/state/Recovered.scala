package com.sos.jobscheduler.core.event.state

import akka.actor.{ActorRef, ActorRefFactory}
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.data.event.{Event, JournaledState}
import com.typesafe.config.Config
import scala.concurrent.duration.FiniteDuration
import shapeless.tag.@@

final class Recovered[S <: JournaledState[S, E], E <: Event](recoverer: JournaledStateRecoverer[S, E], config: Config)
{
  lazy val eventWatch = new JournalEventWatch(recoverer.journalMeta, config)

  def journalMeta: JournalMeta[E] =
    recoverer.journalMeta

  def maybeState: Option[S] =
    recoverer.state

  def totalRunningTime: FiniteDuration =
    recoverer.journalHeader.totalRunningTime

  def startJournalAndFinishRecovery(journalActor: ActorRef @@ JournalActor.type)(implicit arf: ActorRefFactory): Unit =
    recoverer.startJournalAndFinishRecovery(journalActor = journalActor, journalingObserver = Some(eventWatch))
}
