package com.sos.scheduler.engine.agent.process

import com.sos.scheduler.engine.base.exceptions.{StandardPublicException, PublicException}
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.agent.data.responses.{EmptyResponse, Response, StartProcessResponse}
import com.sos.scheduler.engine.agent.process.ProcessHandler._
import com.sos.scheduler.engine.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.scheduler.engine.common.scalautil.{Logger, ScalaConcurrentHashMap}
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import java.time.Instant
import java.time.Instant.now
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

  private val idToAgentProcess = new ScalaConcurrentHashMap[AgentProcessId, AgentProcess] {
    override def default(id: AgentProcessId) = throwUnknownProcess(id)
  }

  def isTerminating = terminating.get
  def terminated = terminatedPromise.future

  def apply(command: Command) = Future[Response] { execute(command) }

  private def execute(command: Command): Response =
    command match {
      case o: StartProcess ⇒ startProcess(o)
      case CloseProcess(id, kill) ⇒ closeProcess(id, kill)
      case SendProcessSignal(id, signal) ⇒
        idToAgentProcess(id).sendProcessSignal(signal)
        EmptyResponse
      case o: Terminate ⇒ terminate(o)
      case AbortImmediately ⇒ haltImmediately()
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
    if (terminating.getAndSet(true)) throw new StandardPublicException("Agent is already terminating")
    if (command.sigtermProcesses) {
      trySigtermProcesses()
    }
    for (t ← command.sigkillProcessesAfter if t < Terminate.MaxDuration) {
      sigkillProcessesAt(now() + t)
    }
    terminateWithProcessesNotBefore(now() + ImmediateTerminationDelay)
    EmptyResponse
  }

  private def trySigtermProcesses(): Unit =
    if (isWindows) {
      logger.debug("Terminate: Under Windows, SIGTERM is ignored")
    } else {
      for (o ← agentProcesses) o.sendProcessSignal(SIGTERM)
    }

  private def sigkillProcessesAt(at: Instant): Unit = {
    logger.info(s"All processes will be terminated with SIGKILL at $at")
    Future {
      blocking {
        sleep(at - now())
      }
      for (o ← agentProcesses) o.sendProcessSignal(SIGKILL)
    }
  }

  private def terminateWithProcessesNotBefore(notBefore: Instant): Unit = {
    Future.sequence(agentProcesses map { _.terminated }) onComplete { o ⇒
      val delay = notBefore - now()
      if (delay > 0.s) {
        // Wait until HTTP request with termination command probably has been responded
        logger.debug(s"Delaying termination for ${delay.pretty}")
        sleep(delay)
      }
      terminatedPromise.complete(o map { _ ⇒ () })
    }
  }

  private def haltImmediately(): Nothing = {
    for (o ← agentProcesses) o.sendProcessSignal(SIGKILL)
    val msg = "Due to command AbortImmediatly, Agent is halted now!"
    logger.warn(msg)
    System.err.println(msg)
    Runtime.getRuntime.halt(1)
    throw new Error("halt")
  }

  def currentProcessCount = idToAgentProcess.size

  def totalProcessCount = totalProcessCounter.get

  def processes = (idToAgentProcess.values map { _.overview }).toVector

  private def agentProcesses = idToAgentProcess.values
}

private object ProcessHandler {
  private val logger = Logger(getClass)
  private val ImmediateTerminationDelay = 1.s  // Allow HTTP with termination command request to be responded

  private def throwUnknownProcess(id: AgentProcessId) = throw new NoSuchElementException(s"Unknown agent process '$id'")
}
