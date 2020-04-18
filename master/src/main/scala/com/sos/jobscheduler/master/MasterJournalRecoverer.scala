package com.sos.jobscheduler.master

import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.{JournaledStateRecoverer, Recovered}
import com.typesafe.config.Config
import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Deadline.now

object MasterJournalRecoverer
{
  def recover(journalMeta: JournalMeta, config: Config, runningSince: Deadline = now): Recovered[MasterState] =
    JournaledStateRecoverer.recover(journalMeta, MasterState.Undefined, () => new MasterStateBuilder, config, runningSince)
}
