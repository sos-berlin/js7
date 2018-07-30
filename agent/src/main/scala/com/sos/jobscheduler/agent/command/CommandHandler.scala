package com.sos.jobscheduler.agent.command

import com.sos.jobscheduler.agent.data.command.{CommandHandlerDetailed, CommandHandlerOverview}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.common.scalautil.Futures.SynchronousExecutionContext
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
trait CommandHandler {

  def execute(command: AgentCommand, meta: CommandMeta = CommandMeta.Default): Future[AgentCommand.Response]

  def overview: Future[CommandHandlerOverview]

  def detailed: Future[CommandHandlerDetailed]

  final def typedExecute(command: AgentCommand, meta: CommandMeta = CommandMeta.Default): Future[command.Response] =
    execute(command, meta) .map { _.asInstanceOf[command.Response]} (SynchronousExecutionContext)
}
