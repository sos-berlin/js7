package com.sos.jobscheduler.agent.task

import akka.actor.{Actor, ActorSystem, Cancellable, DeadLetterSuppression, PoisonPill, Props, Status}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import com.sos.jobscheduler.agent.data.views.TaskRegisterOverview
import com.sos.jobscheduler.agent.data.{AgentTaskId, KillScriptConf}
import com.sos.jobscheduler.agent.task.TaskRegisterActor._
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.data.job.TaskId
import com.typesafe.config.Config
import scala.collection.mutable
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline
import scala.util.control.NonFatal

/**
  * Delivers information about running tasks and handles the `CrashKillScript`.
  *
  * @author Joacim Zschimmer
  */
final class TaskRegisterActor private(killScriptConf: Option[KillScriptConf]) extends Actor {

  import context.dispatcher

  private val idToTask = mutable.Map[AgentTaskId, BaseAgentTask]()
  private var totalCount = 0
  private val crashKillScriptOption =
    for (conf <- killScriptConf) yield new CrashKillScript(conf.killScript, conf.crashKillScriptFile)
  private var killAllSchedule: Cancellable = null
  private var terminating = false

  override def postStop() = {
    if (killAllSchedule != null) {
      killAllSchedule.cancel()
    }
    for (o <- crashKillScriptOption)
      o.close()
    super.postStop()
  }

  def receive = {
    case o: Input => handleInput(o)
    case o: Command => handleCommand(o)
    case o: Internal => handleInternal(o)
  }

  private def handleInput(input: Input): Unit =
    input match {
      case Input.Add(task, promise) =>
        idToTask += task.id -> task
        totalCount += 1
        for (o <- crashKillScriptOption) o.add(task.id, task.pidOption, TaskId(0))
        task.terminated onComplete { _ =>
          self ! Input.Remove(task.id)
        }
        promise.success(Completed)

      case Input.Remove(taskId) =>
        idToTask -= taskId
        for (o <- crashKillScriptOption) o.remove(taskId)
        if (idToTask.isEmpty && terminating) {
          context.stop(self)
        }
    }

  private def handleCommand(command: Command): Unit =
    command match {
      case Command.SendSignalToAllProcesses(signal) =>
        sendSignalToAllProcesses(signal)
        sender() ! Completed

      case _: Command.Terminate if terminating =>
        sender() ! Status.Failure(new IllegalStateException("TaskRegisterActor is already terminating"))

      case cmd: Command.Terminate =>
        terminating = true
        if (cmd.sigterm) {
          trySigtermProcesses()
        }
        killAllSchedule = context.system.scheduler.scheduleOnce(
          delay = cmd.sigkillProcessesDeadline.timeLeftOrZero,
          context.self, Internal.KillAll)
        sender() ! Completed

      case Command.GetOverview =>
        sender() ! TaskRegisterOverview(
          currentTaskCount = idToTask.size,
          totalTaskCount = totalCount)

      case Command.GetTaskOverviews =>
        sender() ! (idToTask.values map { _.overview }).toVector

      case Command.GetTaskOverview(taskId) if idToTask contains taskId =>
        sender() ! idToTask(taskId).overview

      case Command.GetTaskOverview(taskId) =>
        sender() ! Status.Failure(new NoSuchElementException(s"Unknown task $taskId"))
    }

  private def handleInternal(internal: TaskRegisterActor.Internal) =
    internal match {
      case Internal.KillAll =>
        sendSignalToAllProcesses(SIGKILL)
    }

  private def trySigtermProcesses() =
    if (isWindows) {
      logger.debug("ShutDown: Under Windows, SIGTERM is ignored")
    } else {
      sendSignalToAllProcesses(SIGTERM)
    }

  private def sendSignalToAllProcesses(signal: ProcessSignal) =
    for (task <- idToTask.values) {
      try task.sendProcessSignal(signal)
      catch { case NonFatal(t) =>
        logger.warn(s"${task.id}: $t")
      }
    }

  override def toString = s"TaskRegisterActor(${idToTask.size} active tasks, $totalCount total)"
}

object TaskRegisterActor {
  private val logger = Logger(getClass)

  def props(killScriptConf: Option[KillScriptConf]) =
    Props { new TaskRegisterActor(killScriptConf) }
      .withDispatcher("jobscheduler.agent.internal.TaskRegisterActor.mailbox")

  sealed trait Input
  object Input {
    final case class Add(task: BaseAgentTask, response: Promise[Completed]) extends Input
    final case class Remove(taskId: AgentTaskId) extends Input
  }

  sealed trait Command
  object Command {
    final case class SendSignalToAllProcesses(signal: ProcessSignal) extends Command
    final case class Terminate(sigterm: Boolean, sigkillProcessesDeadline: Deadline) extends Command
    final case object GetOverview extends Command
    final case object GetTaskOverviews extends Command
    final case class GetTaskOverview(taskId: AgentTaskId) extends Command
  }

  private sealed trait Internal
  private object Internal {
    final case object KillAll extends Internal with DeadLetterSuppression
  }
}

private[task] final class TaskRegisterActorMailbox(settings: ActorSystem.Settings, config: Config)
extends UnboundedStablePriorityMailbox(
  PriorityGenerator {
    case _: Input.Remove => 0  // Process with priority, to avoid task and process overflow
    case PoisonPill => 1
    case _: Command => 2
    case _: Input.Add => 3
    case _ => 3
  })
