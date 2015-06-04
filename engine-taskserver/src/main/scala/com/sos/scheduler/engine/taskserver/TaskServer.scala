package com.sos.scheduler.engine.taskserver

import com.sos.scheduler.engine.base.process.ProcessSignal

/**
 * A COM server, configured with [[com.sos.scheduler.engine.taskserver.task.StartTaskArguments]], connecting to a controller and
 * executing the controllers task api calls.
 *
 * @author Joacim Zschimmer
 */
trait TaskServer extends AutoCloseable {
  def start(): Unit
  def sendProcessSignal(signal: ProcessSignal): Unit
}
