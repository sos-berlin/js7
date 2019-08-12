package com.sos.jobscheduler.core.event.state

import akka.actor.{ActorRef, ActorRefFactory}
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.JournalRecoverer
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.data.event.{Event, JournaledState}
import com.typesafe.config.Config
import scala.concurrent.duration.FiniteDuration
import shapeless.tag.@@

private final class StateJournalRecoverer[S <: JournaledState[S, E], E <: Event](
  val journalMeta: JournalMeta[E],
  newStateBuilder: () => JournalStateBuilder[S, E])
extends JournalRecoverer[E]
{
  protected val expectedJournalId = None
  private val stateBuilder = newStateBuilder()

  protected def recoverSnapshot =
    stateBuilder.addSnapshot

  override protected def onAllSnapshotRecovered() =
    stateBuilder.onAllSnapshotsAdded()

  protected def recoverEvent =
    stateBuilder.addEvent

  def state: Option[S] =
    hasJournal ? stateBuilder.state(eventId = lastRecoveredEventId)
}

object StateJournalRecoverer
{
  def recover[S <: JournaledState[S, E], E <: Event](
    journalMeta: JournalMeta[E],
    newBuilder: () => JournalStateBuilder[S, E],
    config: Config)
  : Recovered[S, E] = {
    val recoverer = new StateJournalRecoverer(journalMeta, newBuilder)
    recoverer.recoverAll()
    new Recovered(recoverer, config)
  }

  final class Recovered[S <: JournaledState[S, E], E <: Event](recoverer: StateJournalRecoverer[S, E], config: Config)
  {
    lazy val eventWatch = new JournalEventWatch(recoverer.journalMeta, Some(recoverer.journalHeader.journalId), config)

    def journalMeta: JournalMeta[E] =
      recoverer.journalMeta

    def maybeState: Option[S] =
      recoverer.state

    def totalRunningTime: FiniteDuration =
      recoverer.journalHeader.totalRunningTime

    def startJournalAndFinishRecovery(journalActor: ActorRef @@ JournalActor.type)(implicit arf: ActorRefFactory): Unit =
      recoverer.startJournalAndFinishRecovery(journalActor = journalActor, journalingObserver = Some(eventWatch))
  }
}
