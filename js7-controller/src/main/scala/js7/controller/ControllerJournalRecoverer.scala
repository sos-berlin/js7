package js7.controller

import com.typesafe.config.Config
import js7.data.controller.ControllerState
import js7.journal.data.JournalMeta
import js7.journal.recover.{JournaledStateRecoverer, Recovered}
import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Deadline.now

object ControllerJournalRecoverer
{
  def recover(journalMeta: JournalMeta, config: Config, runningSince: Deadline = now): Recovered[ControllerState] =
    JournaledStateRecoverer.recover(journalMeta, config, runningSince)
}
