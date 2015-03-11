package com.sos.scheduler.engine.taskserver

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.taskserver.task.StartConfiguration

/**
 * @author Joacim Zschimmer
 */
private object Main {
  private val logger = Logger(getClass)

  def main(arg: Array[String]): Unit = {
    // <task_process> von stdin lesen
    val startConfiguration: StartConfiguration = ???
    SimpleTaskServer.run(startConfiguration)
  }
}
