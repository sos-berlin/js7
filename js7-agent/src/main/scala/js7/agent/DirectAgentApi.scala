package js7.agent

import js7.agent.command.CommandHandler
import js7.agent.data.commands.AgentCommand
import js7.base.problem.Checked
import js7.core.command.CommandMeta
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
final class DirectAgentApi(commandHandler: CommandHandler, meta: CommandMeta)
{
  def commandExecute(command: AgentCommand): Task[Checked[command.Response]] =
    commandHandler.typedExecute(command, meta)
}
