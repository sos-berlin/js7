package com.sos.jobscheduler.taskserver.modules.common

import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.taskserver.common.StdFiles
import com.sos.jobscheduler.taskserver.modules.monitor.{Monitor, MonitorProcessor}
import com.sos.jobscheduler.taskserver.spoolerapi.TypedNamedIDispatches
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
private[taskserver] final case class CommonArguments(
  agentTaskId: AgentTaskId,
  jobName: String,
  namedIDispatches: TypedNamedIDispatches,
  monitors: immutable.Seq[Monitor],
  hasOrder: Boolean,
  stdFiles: StdFiles)
{
  def newMonitorProcessor(): MonitorProcessor =
    MonitorProcessor.create(monitors, namedIDispatches)
}
