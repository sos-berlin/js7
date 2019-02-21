package com.sos.jobscheduler.agent.command

import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.scalautil.Futures.SynchronousExecutionContext
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.data.command.{CommandHandlerDetailed, CommandHandlerOverview}
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
trait CommandHandler
{
  def execute(command: AgentCommand, meta: CommandMeta = CommandMeta.Anonymous): Future[Checked[AgentCommand.Response]]

  def overview: Future[CommandHandlerOverview]

  def detailed: Future[CommandHandlerDetailed[AgentCommand]]

  final def typedExecute(command: AgentCommand, meta: CommandMeta = CommandMeta.Anonymous): Future[Checked[command.Response]] =
    execute(command, meta) .map { _.asInstanceOf[Checked[command.Response]]} (SynchronousExecutionContext)
}
