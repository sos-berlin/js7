package com.sos.scheduler.engine.agent.process

import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.agent.process.ProcessCommandExecutor._
import com.sos.scheduler.engine.common.scalautil.{Logger, ScalaConcurrentHashMap}
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class ProcessCommandExecutor @Inject private(newAgentProcess: AgentProcessFactory) {

  private val idToAgentProcess = new ScalaConcurrentHashMap[AgentProcessId, AgentProcess] {
    override def default(id: AgentProcessId) = throwUnknownProcess(id)
  }

  def apply(command: ProcessCommand) = Future[Response] {
    command match {
      case command: StartProcess ⇒
        val process = newAgentProcess(command)
        idToAgentProcess += process.id → process
        process.start()
        StartProcessResponse(process.id)

      case CloseProcess(id, kill) ⇒
        val process = idToAgentProcess.remove(id) getOrElse throwUnknownProcess(id)
        if (kill) {
          try process.kill()
          catch { case NonFatal(t) ⇒ logger.warn(s"Kill $process failed: $t") }
        }
        process.close()
        CloseProcessResponse
    }
  }
}

private object ProcessCommandExecutor {
  private val logger = Logger(getClass)

  private def throwUnknownProcess(id: AgentProcessId) = throw new NoSuchElementException(s"Unknown agent process '$id'")
}
