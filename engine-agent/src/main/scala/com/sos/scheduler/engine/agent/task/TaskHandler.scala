package com.sos.scheduler.engine.agent.task

import com.sos.scheduler.engine.agent.command.CommandMeta
import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.agent.data.commandresponses.{EmptyResponse, Response, StartTaskResponse}
import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.agent.data.views.{TaskHandlerOverview, TaskHandlerView, TaskOverview}
import com.sos.scheduler.engine.agent.task.TaskHandler._
import com.sos.scheduler.engine.base.exceptions.StandardPublicException
import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.scheduler.engine.common.scalautil.{Logger, ScalaConcurrentHashMap}
import com.sos.scheduler.engine.common.soslicense.Parameters.UniversalAgent
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.time.Instant
import java.time.Instant.now
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import javax.inject.{Inject, Singleton}
import org.scalactic.Requirements._
import scala.collection.immutable
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

  def execute(command: Command, meta: CommandMeta = CommandMeta()): Future[Response] =
    command match {
      case o: StartTask ⇒ executeStartTask(o, meta)
      case CloseTask(id, kill) ⇒ executeCloseTask(id, kill)
      case SendProcessSignal(id, signal) ⇒ executeSendProcessSignal(id, signal)
      case o: Terminate ⇒ executeTerminate(o)
      case AbortImmediately ⇒ executeAbortImmediately()
    }

  private def executeStartTask(command: StartTask, meta: CommandMeta) = Future {
    if (idToAgentTask.nonEmpty) {
      meta.licenseKeyBunch.require(UniversalAgent, "No license key provided by master to execute jobs in parallel")
    }
    if (isTerminating) throw new StandardPublicException("Agent is terminating and does no longer accept task starts")
    val task = newAgentTask(command, meta.clientIpOption)
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

  private def executeCloseTask(id: AgentTaskId, kill: Boolean) = {
    val task = idToAgentTask.remove(id) getOrElse throwUnknownTask(id)
    if (kill) {
      try task.sendProcessSignal(SIGKILL)
      catch { case NonFatal(t) ⇒
        logger.warn(s"Kill $task failed: $t")
        // Anyway, close Tunnel and TaskServer
        task.close()
      }
    } else {
      task.close()
    }
    val responsePromise = Promise[EmptyResponse.type]()
    task.terminated.onComplete { case o ⇒
      // In case of kill, task has terminated but not yet closed.
      task.close()
      responsePromise.complete(o map { _ ⇒ EmptyResponse })
    }
    responsePromise.future
  }

  private def executeSendProcessSignal(id: AgentTaskId, signal: ProcessSignal) = Future {
    idToAgentTask(id).sendProcessSignal(signal)
    EmptyResponse
  }

  private def executeTerminate(command: Terminate) = Future {
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

  private def executeAbortImmediately(): Nothing = {
    for (o ← agentTasks) o.sendProcessSignal(SIGKILL)
    val msg = "Due to command AbortImmediately, Agent is halted now!"
    logger.warn(msg)
    System.err.println(msg)
    Runtime.getRuntime.halt(1)
    throw new Error("halt")
  }

  def overview = TaskHandlerOverview(
    isTerminating = isTerminating,
    currentTaskCount = idToAgentTask.size,
    totalTaskCount = totalTaskCounter.get)

  def taskOverviews: immutable.Seq[TaskOverview] = (idToAgentTask.values map { _.overview }).toVector

  def taskOverview(id: AgentTaskId) = idToAgentTask(id).overview

  private def agentTasks = idToAgentTask.values
}

private object TaskHandler {
  private val logger = Logger(getClass)
  private val ImmediateTerminationDelay = 1.s  // Allow HTTP with termination command request to be responded

  private def throwUnknownTask(id: AgentTaskId) = throw new NoSuchElementException(s"Unknown agent task '$id'")
}
