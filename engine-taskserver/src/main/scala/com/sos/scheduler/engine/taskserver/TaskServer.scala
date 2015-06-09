package com.sos.scheduler.engine.taskserver

import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments

/**
 * A COM server, configured with [[com.sos.scheduler.engine.taskserver.task.StartTaskArguments]], connecting to a controller and
 * executing the controllers task API calls.
 *
 * @author Joacim Zschimmer
 */
trait TaskServer extends AutoCloseable {
  def taskStartArguments: TaskStartArguments
  def start(): Unit
  def sendProcessSignal(signal: ProcessSignal): Unit
}
