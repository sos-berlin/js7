package js7.tests.jobs

import js7.data.agent.AgentPath
import js7.data.job.InternalExecutable
import js7.data.order.Outcome
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
import monix.eval.Task

final class EmptyJob extends InternalJob
{
  def toOrderProcess(step: Step) =
    OrderProcess(Task.pure(Outcome.succeeded))
}

object EmptyJob
{
  def execute(agentId: AgentPath) =
    Execute(WorkflowJob(agentId, InternalExecutable(classOf[EmptyJob].getName)))
}
