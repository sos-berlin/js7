package com.sos.jobscheduler.master.agent

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.time.JavaTimeConverters._
import com.sos.jobscheduler.core.event.journal.JournalConf
import com.typesafe.config.Config
import scala.concurrent.duration.FiniteDuration

final case class AgentDriverConfiguration(
  eventFetchTimeout: FiniteDuration,
  eventFetchDelay: FiniteDuration,
  eventBufferDelay: FiniteDuration,
  eventBufferLimit: Int,
  commitDelay: FiniteDuration,
  commandBatchSize: Int,
  commandBatchDelay: FiniteDuration,
  commandErrorDelay: FiniteDuration,
  commandParallelism: Int,
  keepEventsPeriod: FiniteDuration)

object AgentDriverConfiguration
{
  def fromConfig(config: Config, journalConf: JournalConf): Checked[AgentDriverConfiguration] =
    Checked.catchNonFatal {
      new AgentDriverConfiguration(
        eventFetchTimeout   = config.getDuration("jobscheduler.master.agent-driver.event-fetch-timeout").toFiniteDuration,
        eventFetchDelay     = config.getDuration("jobscheduler.master.agent-driver.event-fetch-delay").toFiniteDuration,
        eventBufferDelay    = config.getDuration("jobscheduler.master.agent-driver.event-buffer-delay").toFiniteDuration,
        eventBufferLimit    = config.getInt     ("jobscheduler.master.agent-driver.event-buffer-limit"),
        commitDelay         = journalConf.delay,
        commandBatchSize    = config.getInt     ("jobscheduler.master.agent-driver.command-batch-size"),
        commandBatchDelay   = config.getDuration("jobscheduler.master.agent-driver.command-batch-delay").toFiniteDuration,
        commandErrorDelay   = config.getDuration("jobscheduler.master.agent-driver.command-error-delay").toFiniteDuration,
        commandParallelism  = config.getInt     ("jobscheduler.master.agent-driver.command-parallelism"),
        keepEventsPeriod    = config.getDuration("jobscheduler.master.agent-driver.keep-events-period").toFiniteDuration)
    }
}
