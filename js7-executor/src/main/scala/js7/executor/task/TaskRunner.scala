package js7.executor.task

import js7.base.io.process.ProcessSignal
import js7.data.order.{OrderId, Outcome}
import js7.executor.StdChannels
import js7.executor.configuration.TaskConfiguration
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait TaskRunner
{
  def terminate: Task[Unit]

  def processOrder(orderId: OrderId, env: Map[String, String], stdChannels: StdChannels)
  : Task[Outcome.Completed]

  def kill(signal: ProcessSignal): Unit

  def asBaseAgentTask: BaseAgentTask
}

object TaskRunner
{
  trait Factory {
    def apply(conf: TaskConfiguration): TaskRunner
  }
}
