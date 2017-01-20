package com.sos.scheduler.engine.taskserver.modules.monitor

import com.sos.scheduler.engine.taskserver.moduleapi.RawModuleArguments
import com.sos.scheduler.engine.taskserver.modules.monitor.RawMonitorArguments._

/**
  * Arguments for a [[Monitor]] as received from the C++ engine.
  *
  * @author Joacim Zschimmer
  */
private[taskserver] final case class RawMonitorArguments(rawModuleArguments: RawModuleArguments, name: String, ordering: Int = DefaultOrdering)

private[taskserver] object RawMonitorArguments {
  val DefaultOrdering = 1
}
