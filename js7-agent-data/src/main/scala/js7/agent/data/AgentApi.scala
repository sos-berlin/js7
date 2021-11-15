package js7.agent.data

import js7.agent.data.commands.AgentCommand
import js7.agent.data.views.AgentOverview
import js7.base.problem.Checked
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait AgentApi
{
  def commandExecute(command: AgentCommand): Task[Checked[command.Response]]

  //def commandOverview: Task[CommandHandlerOverview]
  //
  //def commandDetailed: Task[CommandHandlerDetailed]

  def overview: Task[AgentOverview]
}
