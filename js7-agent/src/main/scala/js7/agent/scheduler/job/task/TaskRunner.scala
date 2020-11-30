package js7.agent.scheduler.job.task

import com.google.inject.ImplementedBy
import js7.agent.task.BaseAgentTask
import js7.base.process.ProcessSignal
import js7.data.order.OrderId
import js7.taskserver.task.process.StdChannels
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait TaskRunner
{
  def terminate: Task[Unit]

  def processOrder(orderId: OrderId, env: Map[String, String], stdChannels: StdChannels): Task[TaskStepEnded]

  def kill(signal: ProcessSignal): Unit

  def asBaseAgentTask: BaseAgentTask
}

object TaskRunner
{
  @ImplementedBy(classOf[SimpleShellTaskRunner.Factory])
  trait Factory {
    def apply(conf: TaskConfiguration): TaskRunner
  }
}
