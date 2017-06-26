package com.sos.jobscheduler.agent.command

import com.google.inject.ImplementedBy
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
@ImplementedBy(classOf[AgentCommandHandler])
trait CommandExecutor {
  def executeCommand(command: AgentCommand, meta: CommandMeta = CommandMeta()): Future[command.Response]
}
