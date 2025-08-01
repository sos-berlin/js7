package js7.agent

import cats.effect.IO
import js7.agent.data.commands.AgentCommand
import js7.base.problem.Checked
import js7.core.command.CommandMeta

/**
 * @author Joacim Zschimmer
 */
trait CommandHandler:
  def execute(command: AgentCommand, meta: CommandMeta): IO[Checked[AgentCommand.Response]]

  final def typedExecute(command: AgentCommand, meta: CommandMeta): IO[Checked[command.Response]] =
    execute(command, meta).map(_.asInstanceOf[Checked[command.Response]])
