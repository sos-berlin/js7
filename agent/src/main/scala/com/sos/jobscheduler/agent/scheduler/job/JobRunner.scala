package com.sos.jobscheduler.agent.scheduler.job

import akka.actor.{Actor, ActorPath, ActorRef, ActorRefFactory, Props, Stash}
import com.sos.jobscheduler.agent.data.commands.{AbortImmediately, Terminate, TerminateOrAbort}
import com.sos.jobscheduler.agent.scheduler.job.JobRunner._
import com.sos.jobscheduler.agent.scheduler.job.task.ModuleInstanceRunner.{ModuleStepEnded, ModuleStepFailed}
import com.sos.jobscheduler.agent.scheduler.job.task.TaskRunner
import com.sos.jobscheduler.agent.task.AgentTaskFactory
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.jobscheduler.common.akkautils.Akkas.{decodeActorName, encodeAsActorName}
import com.sos.jobscheduler.common.scalautil.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.jobnet.JobPath
import com.sos.jobscheduler.data.order.Order.Bad
import com.sos.jobscheduler.data.order.{Order, OrderId}
import java.nio.file.Path
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
final class JobRunner private(jobPath: JobPath)(implicit newTask: AgentTaskFactory, timerService: TimerService, ec: ExecutionContext)
extends Actor with Stash {

  private val logger = Logger.withPrefix[JobRunner](jobPath.toString)
  private var jobConfiguration: JobConfiguration = null
  private val orderToTask = mutable.Map[OrderId, TaskRunner]()
  private var waitingForNextOrder = false
  private var terminating = false

  def receive = {
    case Command.ReadConfigurationFile(path: Path) ⇒
      jobConfiguration =
        try JobConfiguration.parseXml(jobPath, path)
        catch { case NonFatal(t) ⇒
          logger.error(t.toString, t)
          throw t
        }
      logger.debug("Job is ready")
      context.become(ready)
      sender() ! Response.Ready

    case _: TerminateOrAbort ⇒
      stash()
  }

  private def ready: Receive = {
    case Input.OrderAvailable ⇒
      handleIfReadyForOrder()

    case Command.ProcessOrder(order) if waitingForNextOrder ⇒
      logger.trace(s"ProcessOrder(${order.id})")
      val taskRunner = new TaskRunner(jobConfiguration, newTask)
      orderToTask.insert(order.id → taskRunner)
      waitingForNextOrder = false
      handleIfReadyForOrder()
      val sender = this.sender()
      taskRunner.processOrderAndTerminate(order)
        .onComplete { triedStepEnded ⇒
          self.!(Internal.TaskFinished(order, triedStepEnded))(sender)
        }

    case Internal.TaskFinished(order, triedStepEnded) ⇒
      orderToTask -= order.id
      sender() ! Response.OrderProcessed(order.id, recoverFromFailure(triedStepEnded))
      handleIfReadyForOrder()

    case Terminate(sigtermProcesses, sigkillProcessesAfter) ⇒
      terminating = true
      if (sigtermProcesses) {
        killAll(SIGTERM)
      }
      sigkillProcessesAfter match {
        case Some(duration) ⇒
          timerService.delayedFuture(duration) {
            self.forward(Internal.KillAll)
          }
        case None ⇒
      }

    case AbortImmediately | Internal.KillAll ⇒
      killAll(SIGKILL)
  }

  private def handleIfReadyForOrder() = {
    if (!waitingForNextOrder && !terminating && taskCount < jobConfiguration.taskLimit) {
      context.parent ! Output.ReadyForOrder
      waitingForNextOrder = true
    }
  }

  private def recoverFromFailure(tried: Try[ModuleStepEnded]): ModuleStepEnded =
    tried match {
      case Success(o) ⇒ o
      case Failure(t) ⇒
        logger.error(s"TaskRunner.stepOne failed: $t", t)
        ModuleStepFailed(Bad("TaskRunner.stepOne failed"))
    }

  private def killAll(signal: ProcessSignal): Unit = {
    if (orderToTask.nonEmpty) {
      logger.warn(s"Killing $taskCount tasks")
      for (task ← orderToTask.values) {
        task.kill(signal)
      }
    }
  }

  override def toString = s"JobRunner(${jobPath.string})"

  private def taskCount = orderToTask.size
}

object JobRunner {
  def actorOf(jobPath: JobPath)(implicit actorRefFactory: ActorRefFactory, newTask: AgentTaskFactory, ts: TimerService, ec: ExecutionContext): ActorRef =
    actorRefFactory.actorOf(
      Props { new JobRunner(jobPath) },
      name = toActorName(jobPath))

  def toActorName(o: JobPath): String =
    encodeAsActorName(o.withoutStartingSlash)

  def toJobPath(o: ActorPath): JobPath =
    JobPath("/" + decodeActorName(o.name))


  sealed trait Command
  object Command {
    final case class ReadConfigurationFile(path: Path)
    final case class ProcessOrder(order: Order[Order.InProcess.type]) extends Command
  }

  object Response {
    case object Ready
    final case class OrderProcessed(orderId: OrderId, moduleStepEnded: ModuleStepEnded)
  }

  object Input {
    case object OrderAvailable extends Command
    case object Terminate
  }

  object Output {
    case object ReadyForOrder
  }

  private object Internal {
    final case class TaskFinished(order: Order[Order.State], triedStepEnded: Try[ModuleStepEnded])
    final case object KillAll
  }
}
