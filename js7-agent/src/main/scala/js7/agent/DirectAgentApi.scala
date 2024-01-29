package js7.agent

import cats.effect.IO
import js7.agent.command.CommandHandler
import js7.agent.data.commands.AgentCommand
import js7.base.problem.Checked
import js7.core.command.CommandMeta

/**
  * @author Joacim Zschimmer
  */
final class DirectAgentApi(commandHandler: CommandHandler, meta: CommandMeta):
  def commandExecute(command: AgentCommand): IO[Checked[command.Response]] =
    commandHandler.typedExecute(command, meta)
