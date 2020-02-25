package com.sos.jobscheduler.agent.command

import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.data.command.{CommandHandlerDetailed, CommandHandlerOverview}
import monix.eval.Task

/**
 * @author Joacim Zschimmer
 */
trait CommandHandler
{
  def execute(command: AgentCommand, meta: CommandMeta): Task[Checked[AgentCommand.Response]]

  def overview: Task[CommandHandlerOverview]

  def detailed: Task[CommandHandlerDetailed[AgentCommand]]

  final def typedExecute(command: AgentCommand, meta: CommandMeta): Task[Checked[command.Response]] =
    execute(command, meta) .map { _.asInstanceOf[Checked[command.Response]]}
}
