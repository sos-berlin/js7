package com.sos.scheduler.engine.agent.commandexecutor

import com.sos.scheduler.engine.agent.commandexecutor.AgentCommandExecutor._
import com.sos.scheduler.engine.agent.data.commands.{Command, ProcessCommand, RequestFileOrderSourceContent}
import com.sos.scheduler.engine.agent.fileordersource.RequestFileOrderSourceContentExecutor
import com.sos.scheduler.engine.agent.process.ProcessHandler
import com.sos.scheduler.engine.common.scalautil.Logger
import java.util.concurrent.atomic.AtomicLong
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Executes public Agent commands.
 * @author Joacim Zschimmer
 */
@Singleton
final class AgentCommandExecutor @Inject private(processHandler: ProcessHandler)
extends CommandExecutor {

  private val atomicLong = new AtomicLong(0)

  def executeCommand(command: Command): Future[command.Response] = {
    val number = atomicLong.incrementAndGet()
    logger.info(s"#$number ${command.toShortString}")
    if (command.toStringIsLonger) logger.debug(s"#$number $command")  // Complete string
    val future = command match {
      case command: ProcessCommand ⇒ processHandler.apply(command)
      case command: RequestFileOrderSourceContent ⇒ RequestFileOrderSourceContentExecutor.apply(command)
    }
    future map { response ⇒
      logger.debug(s"Response to #$number ${command.getClass.getSimpleName}: $response")
      response.asInstanceOf[command.Response]
    }
  }
}

object AgentCommandExecutor {
  private val logger = Logger(getClass)
}
