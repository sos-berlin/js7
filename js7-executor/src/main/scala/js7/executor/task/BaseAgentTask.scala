package js7.executor.task

import js7.base.generic.Completed
import js7.base.io.process.ProcessSignal
import js7.base.io.process.Processes.Pid
import js7.data.job.{JobKey, TaskId}
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
trait BaseAgentTask
{
  def id: TaskId

  def jobKey: JobKey

  def pidOption: Option[Pid]

  def terminated: Future[Completed]

  def sendProcessSignal(signal: ProcessSignal): Unit
}
