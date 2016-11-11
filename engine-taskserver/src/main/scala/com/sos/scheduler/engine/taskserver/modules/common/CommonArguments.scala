package com.sos.scheduler.engine.taskserver.modules.common

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.taskserver.common.StdFiles
import com.sos.scheduler.engine.taskserver.modules.monitor.{Monitor, MonitorProcessor}
import com.sos.scheduler.engine.taskserver.spoolerapi.TypedNamedIDispatches
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
