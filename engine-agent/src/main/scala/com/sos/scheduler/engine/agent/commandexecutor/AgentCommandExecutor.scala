package com.sos.scheduler.engine.agent.commandexecutor

import com.sos.scheduler.engine.agent.data.commands.{Command, ProcessCommand, RequestFileOrderSourceContent}
import com.sos.scheduler.engine.agent.process.ProcessCommandExecutor
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Executes public Agent commands.
 * @author Joacim Zschimmer
 */
@Singleton
final class AgentCommandExecutor @Inject private(processCommandExecutor: ProcessCommandExecutor)
extends CommandExecutor {

  def executeCommand(command: Command): Future[command.Response] = {
    val future = command match {
      case command: ProcessCommand ⇒ processCommandExecutor.apply(command)
      case command: RequestFileOrderSourceContent ⇒ RequestFileOrderSourceContentExecutor.apply(command)
    }
    future map { _.asInstanceOf[command.Response] }
  }
}
