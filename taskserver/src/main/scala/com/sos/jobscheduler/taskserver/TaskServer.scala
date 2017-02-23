package com.sos.jobscheduler.taskserver

import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.common.process.Processes.Pid
import com.sos.jobscheduler.taskserver.TaskServer._
import com.sos.jobscheduler.taskserver.data.TaskServerArguments
import scala.concurrent.Future

/**
 * A COM server, configured with [[TaskServerArguments]], connecting to a master and
 * executing the master task API calls.
 *
 * @author Joacim Zschimmer
 */
trait TaskServer extends AutoCloseable {
  def arguments: TaskServerArguments
  def start(): Unit
  def sendProcessSignal(signal: ProcessSignal): Unit
  def deleteLogFiles(): Unit
  def terminated: Future[Terminated.type]
  def pidOption: Option[Pid]

  override def toString = Some(s"master=${arguments.masterAddress})") ++ pidOption mkString (s"${getClass.getSimpleName}(", " ", ")")
}

object TaskServer {

  /**
    * For Future[Terminated], succeeds when TaskServer has terminated.
    */
  object Terminated
}
