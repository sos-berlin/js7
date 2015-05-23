package com.sos.scheduler.engine.agent.commandexecutor

import com.google.inject.ImplementedBy
import com.sos.scheduler.engine.agent.data.commands.Command
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
@ImplementedBy(classOf[AgentCommandExecutor])
trait CommandExecutor {
  def executeCommand(command: Command): Future[command.Response]
}
