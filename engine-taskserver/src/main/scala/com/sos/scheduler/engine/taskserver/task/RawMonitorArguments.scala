package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.taskserver.moduleapi.RawModuleArguments
import com.sos.scheduler.engine.taskserver.task.RawMonitorArguments._

/**
  * Arguments for a [[Monitor]] as received from the C++ engine.
  *
  * @author Joacim Zschimmer
  */
final case class RawMonitorArguments(rawModuleArguments: RawModuleArguments, name: String, ordering: Int = DefaultOrdering)

object RawMonitorArguments {
  val DefaultOrdering = 1
}
