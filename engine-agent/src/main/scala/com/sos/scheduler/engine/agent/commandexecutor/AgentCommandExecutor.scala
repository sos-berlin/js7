package com.sos.scheduler.engine.agent.commandexecutor

import com.sos.scheduler.engine.agent.commands.{Command, ProcessCommand, Response}
import com.sos.scheduler.engine.agent.process.ProcessCommandExecutor
import javax.inject.{Inject, Singleton}
import scala.concurrent.Future

/**
 * Executes public Agent commands.
 * @author Joacim Zschimmer
 */
@Singleton
final class AgentCommandExecutor @Inject private(processCommandExecutor: ProcessCommandExecutor)
extends CommandExecutor {

  def executeCommand(command: Command): Future[Response] =
    command match {
      case command: ProcessCommand ⇒ processCommandExecutor.executeCommand(command)
      //case Terminate ⇒
    }
}
