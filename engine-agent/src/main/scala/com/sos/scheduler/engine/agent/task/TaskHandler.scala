package com.sos.scheduler.engine.agent.task

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.agent.data.commandresponses.{EmptyResponse, Response, StartTaskResponse}
import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.agent.task.TaskHandler._
import com.sos.scheduler.engine.base.exceptions.StandardPublicException
import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.scheduler.engine.common.scalautil.{Logger, ScalaConcurrentHashMap}
import com.sos.scheduler.engine.common.soslicense.LicenseKey
import com.sos.scheduler.engine.common.soslicense.Parameters.UniversalAgent
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.common.time.ScalaTime._
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
final class TaskHandler @Inject private(newAgentTask: AgentTaskFactory) extends TaskHandlerView {

  private val totalTaskCounter = new AtomicInteger(0)
  private val terminating = new AtomicBoolean
  private val terminatedPromise = Promise[Unit]()

  private val idToAgentTask = new ScalaConcurrentHashMap[AgentTaskId, AgentTask] {
    override def default(id: AgentTaskId) = throwUnknownTask(id)
  }

  def isTerminating = terminating.get
  def terminated = terminatedPromise.future

  def execute(command: Command, licenseKey: Option[LicenseKey] = None) = Future[Response] { executeDirectly(command, licenseKey) }

  private def executeDirectly(command: Command, licenseKeyOption: Option[LicenseKey]): Response =
    command match {
      case o: StartTask ⇒ startTask(o, licenseKeyOption getOrElse LicenseKey.Empty)
      case CloseTask(id, kill) ⇒ closeTask(id, kill)
      case SendProcessSignal(id, signal) ⇒
        idToAgentTask(id).sendProcessSignal(signal)
        EmptyResponse
      case o: Terminate ⇒ terminate(o)
      case AbortImmediately ⇒ haltImmediately()
    }

  private def startTask(command: StartTask, licenseKey: LicenseKey) = {
    if (idToAgentTask.nonEmpty) {
      licenseKey.require(UniversalAgent, "Without a license key, a task may only be started if it is the only one")
    }
    if (isTerminating) throw new StandardPublicException("Agent is terminating and does no longer accept task starts")
    val task = newAgentTask(command)
    task.start()
    registerTask(task)
    totalTaskCounter.incrementAndGet()
    StartTaskResponse(task.id, task.tunnelToken)
  }

  private def registerTask(task: AgentTask): Unit = {
    idToAgentTask.synchronized {
      require(!(idToAgentTask contains task.id))
      idToAgentTask += task.id → task
    }
  }

  private def closeTask(id: AgentTaskId, kill: Boolean) = {
    val task = idToAgentTask.remove(id) getOrElse throwUnknownTask(id)
    if (kill) {
      try task.sendProcessSignal(SIGKILL)
      catch { case NonFatal(t) ⇒ logger.warn(s"Kill $task failed: $t") }
    }
    task.close()
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
    terminateWithTasksNotBefore(now() + ImmediateTerminationDelay)
    EmptyResponse
  }

  private def trySigtermProcesses(): Unit =
    if (isWindows) {
      logger.debug("Terminate: Under Windows, SIGTERM is ignored")
    } else {
      sendSignalToAllProcesses(SIGTERM)
    }

  private def sigkillProcessesAt(at: Instant): Unit = {
    logger.info(s"All task processes will be terminated with SIGKILL at $at")
    Future {
      blocking {
        sleep(at - now())
      }
      sendSignalToAllProcesses(SIGKILL)
    }
  }

  private def sendSignalToAllProcesses(signal: ProcessSignal): Unit =
    for (p ← agentTasks) {
      logger.info(s"$signal $p")
      p.sendProcessSignal(signal)
    }

  private def terminateWithTasksNotBefore(notBefore: Instant): Unit = {
    Future.sequence(agentTasks map { _.terminated }) onComplete { o ⇒
      val delay = notBefore - now()
      if (delay > 0.s) {
        // Wait until HTTP request with termination command probably has been responded
        logger.debug(s"Delaying termination for ${delay.pretty}")
        sleep(delay)
      }
      logger.info("Agent is terminating now")
      terminatedPromise.complete(o map { _ ⇒ () })
    }
  }

  private def haltImmediately(): Nothing = {
    for (o ← agentTasks) o.sendProcessSignal(SIGKILL)
    val msg = "Due to command AbortImmediatly, Agent is halted now!"
    logger.warn(msg)
    System.err.println(msg)
    Runtime.getRuntime.halt(1)
    throw new Error("halt")
  }

  def currentTaskCount = idToAgentTask.size

  def totalTaskCount = totalTaskCounter.get

  def tasks = (idToAgentTask.values map { _.overview }).toVector

  private def agentTasks = idToAgentTask.values
}

private object TaskHandler {
  private val logger = Logger(getClass)
  private val ImmediateTerminationDelay = 1.s  // Allow HTTP with termination command request to be responded

  private def throwUnknownTask(id: AgentTaskId) = throw new NoSuchElementException(s"Unknown agent task '$id'")
}
