package js7.master

import com.typesafe.config.Config
import js7.core.event.journal.data.JournalMeta
import js7.core.event.journal.recover.{JournaledStateRecoverer, Recovered}
import js7.master.data.{MasterState, MasterStateBuilder}
import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Deadline.now

object MasterJournalRecoverer
{
  def recover(journalMeta: JournalMeta, config: Config, runningSince: Deadline = now): Recovered[MasterState] =
    JournaledStateRecoverer.recover(journalMeta, MasterState.Undefined, () => new MasterStateBuilder, config, runningSince)
}
