package com.sos.scheduler.engine.taskserver

import com.sos.scheduler.engine.taskserver.task.TaskStartArguments

/**
 * @author Joacim Zschimmer
 */
object Main {
  def main(args: Array[String]): Unit = {
    val startArguments = new TaskStartArguments(controllerAddress = args(1))
    SimpleTaskServer.run(startArguments)
  }
}
