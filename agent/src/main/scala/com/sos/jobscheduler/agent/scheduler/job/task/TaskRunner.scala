package com.sos.jobscheduler.agent.scheduler.job.task

import com.google.inject.ImplementedBy
import com.sos.jobscheduler.agent.task.BaseAgentTask
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.taskserver.task.process.StdChannels
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
trait TaskRunner {

  def terminate(): Future[Completed]

  def processOrder(order: Order[Order.InProcess], stdChannels: StdChannels): Future[TaskStepEnded]

  def kill(signal: ProcessSignal): Unit

  def asBaseAgentTask: BaseAgentTask
}

object TaskRunner {
  @ImplementedBy(classOf[SimpleShellTaskRunner.Factory])
  trait Factory {
    def apply(conf: TaskConfiguration): TaskRunner
  }
}
