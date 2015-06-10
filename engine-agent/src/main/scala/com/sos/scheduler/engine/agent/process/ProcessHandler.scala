package com.sos.scheduler.engine.agent.process

import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.agent.data.responses.{EmptyResponse, Response, StartProcessResponse}
import com.sos.scheduler.engine.agent.process.ProcessHandler._
import com.sos.scheduler.engine.base.process.ProcessSignal.SIGKILL
import com.sos.scheduler.engine.common.scalautil.{Logger, ScalaConcurrentHashMap}
import java.util.concurrent.atomic.AtomicInteger
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class ProcessHandler @Inject private(newAgentProcess: AgentProcessFactory) extends ProcessHandlerView {

  private val totalProcessCounter = new AtomicInteger(0)

  private val idToAgentProcess = new ScalaConcurrentHashMap[AgentProcessId, AgentProcess] {
    override def default(id: AgentProcessId) = throwUnknownProcess(id)
  }

  def apply(command: Command) = Future[Response] { execute(command) }

  private def execute(command: Command): Response =
    command match {
      case command: StartProcess ⇒
        val process = newAgentProcess(command)
        idToAgentProcess += process.id → process
        process.start()
        totalProcessCounter.incrementAndGet()
        StartProcessResponse(process.id)

      case CloseProcess(id, kill) ⇒
        val process = idToAgentProcess.remove(id) getOrElse throwUnknownProcess(id)
        if (kill) {
          try process.sendProcessSignal(SIGKILL)
          catch { case NonFatal(t) ⇒ logger.warn(s"Kill $process failed: $t") }
        }
        process.close()
        EmptyResponse

      case SendProcessSignal(id, signal) ⇒
        idToAgentProcess(id).sendProcessSignal(signal)
        EmptyResponse
    }

  def currentProcessCount = idToAgentProcess.size

  def totalProcessCount = totalProcessCounter.get

  def processes = (idToAgentProcess.values map { _.overview }).toVector
}

private object ProcessHandler {
  private val logger = Logger(getClass)

  private def throwUnknownProcess(id: AgentProcessId) = throw new NoSuchElementException(s"Unknown agent process '$id'")
}
