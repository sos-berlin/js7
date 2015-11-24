package com.sos.scheduler.engine.taskserver

import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.taskserver.data.TaskStartArguments
import com.sos.scheduler.engine.taskserver.task.process.Processes.Pid
import scala.concurrent.Future

/**
 * A COM server, configured with [[TaskStartArguments]], connecting to a master and
 * executing the master task API calls.
 *
 * @author Joacim Zschimmer
 */
trait TaskServer extends AutoCloseable {
  def taskStartArguments: TaskStartArguments
  def start(): Unit
  def sendProcessSignal(signal: ProcessSignal): Unit
  def deleteLogFiles(): Unit
  def terminated: Future[Unit]
  def pidOption: Option[Pid]

  override def toString = Some(s"master=${taskStartArguments.masterAddress})") ++ pidOption mkString (s"${getClass.getSimpleName}(", " ", ")")
}
