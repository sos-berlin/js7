package com.sos.jobscheduler.agent.task

import akka.actor.{Actor, Cancellable, Props, Status}
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.views.{TaskOverview, TaskRegisterOverview}
import com.sos.jobscheduler.agent.task.TaskRegister._
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
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
private class TaskRegister(terminated: Promise[Completed], agentConfiguration: AgentConfiguration, timerService: TimerService) extends Actor {

  import context.dispatcher

  private val idToTask = mutable.Map[AgentTaskId, BaseAgentTask]()
  private var totalCount = 0
  private val crashKillScriptOption = for (script ← agentConfiguration.killScript if agentConfiguration.crashKillScriptEnabled)
    yield new CrashKillScript(script, agentConfiguration.crashKillScriptFile)
  private var killAllSchedule: Cancellable = null

  override def preRestart(throwable: Throwable, message: Option[Any]) = {
    terminated.failure(throwable)
    super.preRestart(throwable, message)
  }

  override def postStop() = {
    terminated.trySuccess(Completed)
    if (killAllSchedule != null) {
      killAllSchedule.cancel()
    }
    super.postStop()
  }

  def receive = {
    case o: Command ⇒ handleCommand(o)
    case o: Input ⇒ handleInput(o)
    case o: Internal ⇒ handleInternal(o)
  }

  private def handleCommand(command: Command): Unit =
    command match {
      case Command.SendSignalToAllProcesses(signal) ⇒
        sendSignalToAllProcesses(signal)
        sender() ! Response.OK

      case cmd: Command.Terminate ⇒
        if (cmd.sigterm) {
          trySigtermProcesses()
        }
        val sender = this.sender()
        killAllSchedule = context.system.scheduler.scheduleOnce((cmd.sigkillProcessesAfter - now max 0.s).toFiniteDuration, context.self, Internal.KillAll)
        Future.sequence(idToTask.values map { _.terminated }) onComplete { _ ⇒
          context.stop(self)
          sender ! Response.OK
        }

      case Command.GetOverview ⇒
        sender() ! Response.GotOverview(TaskRegisterOverview(
          currentTaskCount = idToTask.size,
          totalTaskCount = totalCount))

      case Command.GetTaskOverviews ⇒
        sender() ! Response.GotTaskOverviews((idToTask.values map { _.overview }).toVector)

      case Command.GetTaskOverview(taskId) if idToTask contains taskId ⇒
        sender() ! Response.GotTaskOverview(idToTask(taskId).overview)

      case Command.GetTaskOverview(taskId) ⇒
        sender() ! Status.Failure(new NoSuchElementException(s"Unknown task $taskId"))
    }

  private def handleInput(input: Input): Unit =
    input match {
      case Input.Add(task) ⇒
        idToTask += task.id → task
        totalCount += 1
        for (o ← crashKillScriptOption) o.add(task.id, task.pidOption, TaskId(0), task.jobPath)

      case Input.Remove(taskId) ⇒
        idToTask -= taskId
        for (o ← crashKillScriptOption) o.remove(taskId)

      case Input.KillProcessesAt(at: Instant) ⇒
        logger.info(s"All task processes will be terminated with SIGKILL at $at")
        timerService.at(at, "SIGKILL all processes") onElapsed {
          sendSignalToAllProcesses(SIGKILL)
        }
    }

  private def handleInternal(internal: TaskRegister.Internal) =
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
}

object TaskRegister {
  private val logger = Logger(getClass)

  def props(terminated: Promise[Completed])(implicit agentConfiguration: AgentConfiguration, timerService: TimerService) =
    Props { new TaskRegister(terminated, agentConfiguration, timerService) }

  sealed trait Command
  object Command {
    final case class SendSignalToAllProcesses(signal: ProcessSignal) extends Command
    final case class Terminate(sigterm: Boolean, sigkillProcessesAfter: Instant) extends Command
    final case object GetOverview extends Command
    final case object GetTaskOverviews extends Command
    final case class GetTaskOverview(taskId: AgentTaskId) extends Command
  }

  sealed trait Input
  object Input {
    final case class Add(task: BaseAgentTask) extends Input
    final case class Remove(taskId: AgentTaskId) extends Input
    final case class KillProcessesAt(at: Instant) extends Input
  }

  sealed trait Response
  object Response {
    final case object OK
    final case class GotOverview(overview: TaskRegisterOverview) extends Response
    final case class GotTaskOverviews(overviews: Seq[TaskOverview]) extends Response
    final case class GotTaskOverview(overview: TaskOverview) extends Response
  }

  private sealed trait Internal
  private object Internal {
    final case object KillAll extends Internal
  }
}
