package com.sos.jobscheduler.core.event.state

import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.JournalRecoverer
import com.sos.jobscheduler.data.event.{Event, JournaledState}
import com.typesafe.config.Config

private final class JournaledStateRecoverer[S <: JournaledState[S, E], E <: Event](
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

object JournaledStateRecoverer
{
  def recover[S <: JournaledState[S, E], E <: Event](
    journalMeta: JournalMeta[E],
    newBuilder: () => JournalStateBuilder[S, E],
    config: Config)
  : Recovered[S, E] = {
    val recoverer = new JournaledStateRecoverer(journalMeta, newBuilder)
    recoverer.recoverAll()
    new Recovered(recoverer, config)
  }
}
