package com.sos.scheduler.engine.agent.process

import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.agent.data.responses.{EmptyResponse, Response, StartProcessResponse}
import com.sos.scheduler.engine.agent.process.ProcessHandler._
import com.sos.scheduler.engine.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.scheduler.engine.common.scalautil.{Logger, ScalaConcurrentHashMap}
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import javax.inject.{Inject, Singleton}
import org.scalactic.Requirements._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise, blocking}
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class ProcessHandler @Inject private(newAgentProcess: AgentProcessFactory) extends ProcessHandlerView {

  private val totalProcessCounter = new AtomicInteger(0)
  private val terminating = new AtomicBoolean
  private val terminatedPromise = Promise[Unit]()
  def terminated = terminatedPromise.future

  private val idToAgentProcess = new ScalaConcurrentHashMap[AgentProcessId, AgentProcess] {
    override def default(id: AgentProcessId) = throwUnknownProcess(id)
  }

  def apply(command: Command) = Future[Response] { execute(command) }

  private def execute(command: Command): Response =
    command match {
      case o: StartProcess ⇒ startProcess(o)
      case CloseProcess(id, kill) ⇒ closeProcess(id, kill)
      case SendProcessSignal(id, signal) ⇒
        idToAgentProcess(id).sendProcessSignal(signal)
        EmptyResponse
      case o: Terminate ⇒ terminate(o)
    }

  private def startProcess(command: StartProcess) = {
    val process = newAgentProcess(command)
    process.start()
    registerProcess(process)
    totalProcessCounter.incrementAndGet()
    StartProcessResponse(process.id)
  }

  private def registerProcess(process: AgentProcess): Unit = {
    idToAgentProcess.synchronized {
      require(!(idToAgentProcess contains process.id))
      idToAgentProcess += process.id → process
    }
  }

  private def closeProcess(id: AgentProcessId, kill: Boolean) = {
    val process = idToAgentProcess.remove(id) getOrElse throwUnknownProcess(id)
    if (kill) {
      try process.sendProcessSignal(SIGKILL)
      catch { case NonFatal(t) ⇒ logger.warn(s"Kill $process failed: $t") }
    }
    process.close()
    EmptyResponse
  }

  private def terminate(command: Terminate) = {
    val wasTerminating = terminating.getAndSet(true)
    if (command == Terminate.AbortImmediately) {
      for (o ← agentProcesses) o.sendProcessSignal(SIGKILL)
      abortImmediately()
    }
    if (wasTerminating) sys.error("Agent is already terminating")
    if (command.sigtermProcesses) {
      if (isWindows) {
        logger.debug("Terminate: Under Windows, SIGTERM is ignored")
      } else {
        for (o ← agentProcesses) o.sendProcessSignal(SIGTERM)
      }
    }
    if (command.sigkillProcessesAfter < Terminate.MaxDuration) {
      Future {
        blocking {
          sleep(command.sigkillProcessesAfter)
        }
        for (o ← agentProcesses) o.sendProcessSignal(SIGKILL)
      }
    }
    Future.sequence(agentProcesses map { _.terminated }) onComplete { o ⇒ terminatedPromise.complete(o map { _ ⇒ () }) }
    EmptyResponse
  }

  def currentProcessCount = idToAgentProcess.size

  def totalProcessCount = totalProcessCounter.get

  def processes = (idToAgentProcess.values map { _.overview }).toVector

  private def agentProcesses = idToAgentProcess.values
}

private object ProcessHandler {
  private val logger = Logger(getClass)

  private def throwUnknownProcess(id: AgentProcessId) = throw new NoSuchElementException(s"Unknown agent process '$id'")

  private def abortImmediately(): Unit = {
    val msg = "Due to command AbortImmediatly, Agent is halted now!"
    logger.warn(msg)
    System.err.println(msg)
    Runtime.getRuntime.halt(1)
  }
}
