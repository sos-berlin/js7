package com.sos.scheduler.engine.agent

import com.sos.scheduler.engine.agent.commands.{Command, Response}
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
trait CommandExecutor {
  def executeCommand(command: Command): Future[Response]
}
