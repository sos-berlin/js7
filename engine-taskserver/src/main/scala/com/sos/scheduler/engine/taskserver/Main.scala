package com.sos.scheduler.engine.taskserver

import com.sos.scheduler.engine.taskserver.task.TaskStartArguments

/**
 * @author Joacim Zschimmer
 */
object Main {
  def main(args: Array[String]): Unit = {
    try {
      val startArguments = new TaskStartArguments(controllerAddress = args(0) stripPrefix "-controller=")
      SimpleTaskServer.run(startArguments)
    } catch {
      case t: Throwable ⇒
        System.err.println(t.toString)
        t.printStackTrace(System.err)
        System.exit(1)
    }
  }
}
