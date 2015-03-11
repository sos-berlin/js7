package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.taskserver.module.Module
import com.sos.scheduler.engine.taskserver.task.Monitor._

/**
 * @author Joacim Zschimmer
 */
final case class Monitor(module: Module, name: String, ordering: Int = DefaultOrdering)

object Monitor {
  val DefaultOrdering = 1
}
