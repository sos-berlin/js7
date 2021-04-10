package js7.tests.jobs

import js7.data.agent.AgentId
import js7.data.job.InternalExecutable
import js7.data.order.Outcome
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
import monix.eval.Task

final class EmptyJob extends InternalJob
{
  def processOrder(step: Step) =
    OrderProcess(Task.pure(Outcome.succeeded))
}

object EmptyJob
{
  def execute(agentId: AgentId) =
    Execute(WorkflowJob(agentId, InternalExecutable(classOf[EmptyJob].getName)))
}
