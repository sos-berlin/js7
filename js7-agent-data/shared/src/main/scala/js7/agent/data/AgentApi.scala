package js7.agent.data

import js7.agent.data.commands.AgentCommand
import js7.base.problem.Checked
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait AgentApi
{
  def commandExecute(command: AgentCommand): Task[Checked[command.Response]]
}
