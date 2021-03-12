package js7.tests.jobs

import js7.data.agent.AgentId
import js7.data.job.InternalExecutable
import js7.data.value.NamedValues
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.executor.internal.InternalJob
import js7.executor.internal.InternalJob.{OrderContext, OrderProcess, Result}
import monix.eval.Task

final class EmptyJob extends InternalJob
{
  def processOrder(context: OrderContext) =
    OrderProcess(
      Task.pure(Right(
        Result(NamedValues.empty))))
}

object EmptyJob
{
  def execute(agentId: AgentId) =
    Execute(WorkflowJob(agentId, InternalExecutable(classOf[EmptyJob].getName)))
}
