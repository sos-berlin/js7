package com.sos.jobscheduler.agent.task

import akka.actor.{Actor, Cancellable, DeadLetterSuppression, Status}
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.views.TaskRegisterOverview
import com.sos.jobscheduler.agent.task.TaskRegisterActor._
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.job.TaskId
import java.time.Instant
import java.time.Instant.now
import scala.collection.mutable
import scala.util.control.NonFatal

/**
  * Delivers information about running tasks and handles the `CrashKillScript`.
  *
  * @author Joacim Zschimmer
  */
final class TaskRegisterActor(agentConfiguration: AgentConfiguration, timerService: TimerService) extends Actor {

  import context.dispatcher

  private val idToTask = mutable.Map[AgentTaskId, BaseAgentTask]()
  private var totalCount = 0
  private val crashKillScriptOption = for (script ← agentConfiguration.killScript if agentConfiguration.crashKillScriptEnabled)
    yield new CrashKillScript(script, agentConfiguration.crashKillScriptFile)
  private var killAllSchedule: Cancellable = null
  private var terminating = false

  override def postStop() = {
    if (killAllSchedule != null) {
      killAllSchedule.cancel()
    }
    super.postStop()
  }

  def receive = {
    case o: Input ⇒ handleInput(o)
    case o: Command ⇒ handleCommand(o)
    case o: Internal ⇒ handleInternal(o)
  }

  private def handleInput(input: Input): Unit =
    input match {
      case Input.Add(task) ⇒
        idToTask += task.id → task
        totalCount += 1
        for (o ← crashKillScriptOption) o.add(task.id, task.pidOption, TaskId(0), task.jobPath)
        task.terminated onComplete { _ ⇒
          self ! Input.Remove(task.id)
        }

      case Input.Remove(taskId) ⇒
        idToTask -= taskId
        for (o ← crashKillScriptOption) o.remove(taskId)
        if (idToTask.isEmpty && terminating) {
          context.stop(self)
        }
    }

  private def handleCommand(command: Command): Unit =
    command match {
      case Command.SendSignalToAllProcesses(signal) ⇒
        sendSignalToAllProcesses(signal)
        sender() ! Completed

      case _: Command.Terminate if terminating ⇒
        sender() ! Status.Failure(new IllegalStateException("TaskRegisterActor is already terminating"))

      case cmd: Command.Terminate ⇒
        terminating = true
        if (cmd.sigterm) {
          trySigtermProcesses()
        }
        killAllSchedule = context.system.scheduler.scheduleOnce(
          delay = (cmd.sigkillProcessesAfter - now max 0.s).toFiniteDuration,
          context.self, Internal.KillAll)
        sender() ! Completed

      case Command.GetOverview ⇒
        sender() ! TaskRegisterOverview(
          currentTaskCount = idToTask.size,
          totalTaskCount = totalCount)

      case Command.GetTaskOverviews ⇒
        sender() ! (idToTask.values map { _.overview }).toVector

      case Command.GetTaskOverview(taskId) if idToTask contains taskId ⇒
        sender() ! idToTask(taskId).overview

      case Command.GetTaskOverview(taskId) ⇒
        sender() ! Status.Failure(new NoSuchElementException(s"Unknown task $taskId"))
    }

  private def handleInternal(internal: TaskRegisterActor.Internal) =
    internal match {
      case Internal.KillAll ⇒
        sendSignalToAllProcesses(SIGKILL)
    }

  private def trySigtermProcesses() =
    if (isWindows) {
      logger.debug("Terminate: Under Windows, SIGTERM is ignored")
    } else {
      sendSignalToAllProcesses(SIGTERM)
    }

  private def sendSignalToAllProcesses(signal: ProcessSignal) =
    for (task ← idToTask.values) {
      try task.sendProcessSignal(signal)
      catch { case NonFatal(t) ⇒
        logger.warn(s"${task.id}: $t")
      }
    }

  override def toString = s"TaskRegisterActor(${idToTask.size} active tasks, $totalCount total)"
}

private[task] object TaskRegisterActor {
  private val logger = Logger(getClass)

  sealed trait Input
  object Input {
    final case class Add(task: BaseAgentTask) extends Input
    final case class Remove(taskId: AgentTaskId) extends Input
  }

  sealed trait Command
  object Command {
    final case class SendSignalToAllProcesses(signal: ProcessSignal) extends Command
    final case class Terminate(sigterm: Boolean, sigkillProcessesAfter: Instant) extends Command
    final case object GetOverview extends Command
    final case object GetTaskOverviews extends Command
    final case class GetTaskOverview(taskId: AgentTaskId) extends Command
  }

  private sealed trait Internal
  private object Internal {
    final case object KillAll extends Internal with DeadLetterSuppression
  }
}
