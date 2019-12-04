package com.sos.jobscheduler.master

import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.{JournaledStateRecoverer, Recovered}
import com.sos.jobscheduler.data.event.Event
import com.typesafe.config.Config
import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Deadline.now

object MasterJournalRecoverer
{
  def recover(journalMeta: JournalMeta, config: Config, runningSince: Deadline = now): Recovered[MasterState, Event] =
    JournaledStateRecoverer.recover[MasterState, Event](journalMeta, () => new MasterStateBuilder, config, runningSince)
}
