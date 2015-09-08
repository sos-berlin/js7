package com.sos.scheduler.engine.taskserver

import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.taskserver.data.TaskStartArguments
import scala.concurrent.Future

/**
 * A COM server, configured with [[TaskStartArguments]], connecting to a controller and
 * executing the controllers task API calls.
 *
 * @author Joacim Zschimmer
 */
trait TaskServer extends AutoCloseable {
  def taskStartArguments: TaskStartArguments
  def start(): Unit
  def sendProcessSignal(signal: ProcessSignal): Unit
  def terminated: Future[Unit]
}
