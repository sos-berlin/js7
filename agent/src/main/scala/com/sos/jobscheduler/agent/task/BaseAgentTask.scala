package com.sos.jobscheduler.agent.task

import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.views.TaskOverview
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.common.process.Processes.Pid
import com.sos.jobscheduler.data.job.JobPath
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
trait BaseAgentTask {

  def id: AgentTaskId

  def jobPath: JobPath

  def pidOption: Option[Pid]

  def terminated: Future[Completed]

  def sendProcessSignal(signal: ProcessSignal): Unit

  def overview: TaskOverview
}

