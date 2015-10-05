package com.sos.scheduler.engine.agent.command

import com.google.inject.ImplementedBy
import com.sos.scheduler.engine.agent.data.commands.Command
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
@ImplementedBy(classOf[AgentCommandHandler])
trait CommandExecutor {
  def executeCommand(command: Command, meta: CommandMeta = CommandMeta()): Future[command.Response]
}
