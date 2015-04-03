package com.sos.scheduler.engine.agent.commandexecutor

import com.google.inject.ImplementedBy
import com.sos.scheduler.engine.agent.commands.{Command, Response}
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
@ImplementedBy(classOf[AgentCommandExecutor])
trait CommandExecutor {
  def executeCommand(command: Command): Future[Response]
}
