package js7.agent.scheduler.job.task

import com.google.inject.ImplementedBy
import js7.agent.task.BaseAgentTask
import js7.base.generic.Completed
import js7.base.process.ProcessSignal
import js7.data.order.Order
import js7.taskserver.task.process.StdChannels
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
trait TaskRunner {

  def terminate(): Future[Completed]

  def processOrder(order: Order[Order.Processing], defaultArguments: Map[String, String], stdChannels: StdChannels): Future[TaskStepEnded]

  def kill(signal: ProcessSignal): Unit

  def asBaseAgentTask: BaseAgentTask
}

object TaskRunner {
  @ImplementedBy(classOf[SimpleShellTaskRunner.Factory])
  trait Factory {
    def apply(conf: TaskConfiguration): TaskRunner
  }
}
