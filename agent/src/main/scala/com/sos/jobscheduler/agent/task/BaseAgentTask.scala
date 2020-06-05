package js7.agent.task

import js7.agent.data.AgentTaskId
import js7.agent.data.views.TaskOverview
import js7.base.generic.Completed
import js7.base.process.ProcessSignal
import js7.common.process.Processes.Pid
import js7.data.job.JobKey
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
trait BaseAgentTask {

  def id: AgentTaskId

  def jobKey: JobKey

  def pidOption: Option[Pid]

  def terminated: Future[Completed]

  def sendProcessSignal(signal: ProcessSignal): Unit

  def overview: TaskOverview
}

