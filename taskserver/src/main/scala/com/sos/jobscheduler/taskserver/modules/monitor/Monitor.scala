package com.sos.jobscheduler.taskserver.modules.monitor

import com.sos.jobscheduler.taskserver.moduleapi.ModuleArguments
import com.sos.jobscheduler.taskserver.modules.monitor.Monitor._

/**
 * @author Joacim Zschimmer
 */
private[taskserver] final case class Monitor(moduleArguments: ModuleArguments, name: String, ordering: Int = DefaultOrdering)

private[taskserver] object Monitor {
  val DefaultOrdering = 1
}
