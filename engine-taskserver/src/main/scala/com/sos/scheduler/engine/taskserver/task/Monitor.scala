package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.taskserver.module.ModuleArguments
import com.sos.scheduler.engine.taskserver.task.Monitor._

/**
 * @author Joacim Zschimmer
 */
final case class Monitor(moduleArguments: ModuleArguments, name: String, ordering: Int = DefaultOrdering)

object Monitor {
  val DefaultOrdering = 1
}
