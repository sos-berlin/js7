package com.sos.jobscheduler.master.agent

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.time.JavaTimeConverters._
import com.typesafe.config.Config
import scala.concurrent.duration.FiniteDuration

final case class AgentDriverConfiguration(
  eventFetchTimeout: FiniteDuration,
  eventFetchDelay: FiniteDuration,
  eventBufferDuration: FiniteDuration,
  eventBufferLimit: Int,
  commandBatchSize: Int,
  commandBatchDelay: FiniteDuration,
  commandErrorDelay: FiniteDuration,
  commandParallelism: Int,
  keepEventsPeriod: FiniteDuration)

object AgentDriverConfiguration
{
  def fromConfig(config: Config): Checked[AgentDriverConfiguration] =
    Checked.catchNonFatal {
      new AgentDriverConfiguration(
        eventFetchTimeout   = config.getDuration("jobscheduler.master.agent-driver.event-fetch-timeout").toFiniteDuration,
        eventFetchDelay     = config.getDuration("jobscheduler.master.agent-driver.event-fetch-delay").toFiniteDuration,
        eventBufferDuration = config.getDuration("jobscheduler.master.agent-driver.event-buffer-duration").toFiniteDuration,
        eventBufferLimit    = config.getInt     ("jobscheduler.master.agent-driver.event-buffer-limit"),
        commandBatchSize    = config.getInt     ("jobscheduler.master.agent-driver.command-batch-size"),
        commandBatchDelay   = config.getDuration("jobscheduler.master.agent-driver.command-batch-delay").toFiniteDuration,
        commandErrorDelay   = config.getDuration("jobscheduler.master.agent-driver.command-error-delay").toFiniteDuration,
        commandParallelism  = config.getInt     ("jobscheduler.master.agent-driver.command-parallelism"),
        keepEventsPeriod    = config.getDuration("jobscheduler.master.agent-driver.keep-events-period").toFiniteDuration)
    }
}
