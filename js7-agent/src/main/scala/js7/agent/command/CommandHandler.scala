package js7.agent.command

import js7.agent.data.commands.AgentCommand
import js7.base.problem.Checked
import js7.core.command.CommandMeta
import monix.eval.Task

/**
 * @author Joacim Zschimmer
 */
trait CommandHandler
{
  def execute(command: AgentCommand, meta: CommandMeta): Task[Checked[AgentCommand.Response]]

  final def typedExecute(command: AgentCommand, meta: CommandMeta): Task[Checked[command.Response]] =
    execute(command, meta).map(_.asInstanceOf[Checked[command.Response]])
}
