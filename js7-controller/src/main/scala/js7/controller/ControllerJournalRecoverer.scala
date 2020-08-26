package js7.controller

import com.typesafe.config.Config
import js7.controller.data.ControllerState
import js7.core.event.journal.data.JournalMeta
import js7.core.event.journal.recover.{JournaledStateRecoverer, Recovered}
import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Deadline.now

object ControllerJournalRecoverer
{
  def recover(journalMeta: JournalMeta, config: Config, runningSince: Deadline = now): Recovered[ControllerState] =
    JournaledStateRecoverer.recover(journalMeta, config, runningSince)
}
