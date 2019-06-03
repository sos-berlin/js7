package com.sos.jobscheduler.master.agent

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.time.JavaTimeConverters._
import com.typesafe.config.Config
import scala.concurrent.duration.FiniteDuration

final case class AgentDriverConfiguration(
  batchSize: Int,
  batchDelay: FiniteDuration,
  keepEventsPeriod: FiniteDuration,
  eventLimit: Int,
  eventFetchTimeout: FiniteDuration,
  eventFetchDelay: FiniteDuration,
  eventTimeoutDelay: FiniteDuration)

object AgentDriverConfiguration
{
  def fromConfig(config: Config): Checked[AgentDriverConfiguration] =
    Checked.catchNonFatal {
      new AgentDriverConfiguration(
        batchSize         = config.getInt     ("jobscheduler.master.agent-driver.command-batch-size"),
        batchDelay        = config.getDuration("jobscheduler.master.agent-driver.command-batch-delay").toFiniteDuration,
        keepEventsPeriod  = config.getDuration("jobscheduler.master.agent-driver.keep-events-period").toFiniteDuration,
        eventLimit        = config.getInt     ("jobscheduler.master.agent-driver.event-fetch-limit"),
        eventFetchTimeout = config.getDuration("jobscheduler.master.agent-driver.event-fetch-timeout").toFiniteDuration,
        eventFetchDelay   = config.getDuration("jobscheduler.master.agent-driver.event-fetch-delay").toFiniteDuration,
        eventTimeoutDelay = config.getDuration("jobscheduler.master.agent-driver.event-timeout-delay").toFiniteDuration)
    }
}
