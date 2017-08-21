package com.sos.jobscheduler.agent.scheduler.job

import akka.actor.{Actor, ActorPath, ActorRef, ActorRefFactory, Props, Stash}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.scheduler.job.JobRunner._
import com.sos.jobscheduler.agent.scheduler.job.task.{TaskRunner, TaskStepEnded, TaskStepFailed}
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.jobscheduler.common.akkautils.Akkas.{decodeActorName, encodeAsActorName}
import com.sos.jobscheduler.common.scalautil.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.jobnet.JobPath
import com.sos.jobscheduler.data.order.Order.Bad
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.taskserver.task.process.StdChannels
import java.nio.file.Path
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
final class JobRunner private(jobPath: JobPath, newTaskRunner: TaskRunner.Factory)(implicit timerService: TimerService, ec: ExecutionContext)
extends Actor with Stash {

  private val logger = Logger.withPrefix[JobRunner](jobPath.toString)
  private var jobConfiguration: JobConfiguration = null
  private val orderToTask = mutable.Map[OrderId, TaskRunner]()
  private var waitingForNextOrder = false
  private var terminating = false

  def receive = {
    case Command.StartWithConfigurationFile(path: Path) ⇒
      val conf = try JobConfiguration.parseXml(jobPath, path)
        catch { case NonFatal(t) ⇒
          logger.error(t.toString, t)
          throw t
        }
      self.forward(Command.StartWithConfiguration(conf))

    case Command.StartWithConfiguration(conf) ⇒
      jobConfiguration = conf
      logger.debug("Job is ready")
      context.become(ready)
      sender() ! Response.Ready

    case _: AgentCommand.TerminateOrAbort ⇒
      stash()
  }

  private def ready: Receive = {
    case Input.OrderAvailable ⇒
      handleIfReadyForOrder()

    case Command.ProcessOrder(order, stdoutStderrWriter) if waitingForNextOrder ⇒
      logger.trace(s"ProcessOrder(${order.id})")
      val taskRunner = newTaskRunner(jobConfiguration)
      orderToTask.insert(order.id → taskRunner)
      waitingForNextOrder = false
      handleIfReadyForOrder()
      val sender = this.sender()
      taskRunner.processOrder(order, stdoutStderrWriter)
        .andThen { case _ ⇒ taskRunner.terminate() }
        .onComplete { triedStepEnded ⇒
          self.!(Internal.TaskFinished(order, triedStepEnded))(sender)
        }

    case Internal.TaskFinished(order, triedStepEnded) ⇒
      orderToTask -= order.id
      sender() ! Response.OrderProcessed(order.id, recoverFromFailure(triedStepEnded))
      handleActorStop()
      handleIfReadyForOrder()

    case AgentCommand.Terminate(sigtermProcesses, sigkillProcessesAfter) ⇒
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
      handleActorStop()

    case Internal.KillAll ⇒
      killAll(SIGKILL)
  }

  private def handleIfReadyForOrder() = {
    if (!waitingForNextOrder && !terminating && taskCount < jobConfiguration.taskLimit) {
      context.parent ! Output.ReadyForOrder
      waitingForNextOrder = true
    }
  }

  private def recoverFromFailure(tried: Try[TaskStepEnded]): TaskStepEnded =
    tried match {
      case Success(o) ⇒ o
      case Failure(t) ⇒
        logger.error(s"TaskRunner.stepOne failed: $t", t)
        TaskStepFailed(Bad("TaskRunner.stepOne failed"))
    }

  private def killAll(signal: ProcessSignal): Unit = {
    if (orderToTask.nonEmpty) {
      logger.warn(s"Terminating, sending $signal to all $taskCount tasks")
      for (task ← orderToTask.values) {
        task.kill(signal)
      }
    }
  }

  private def handleActorStop(): Unit = {
    if (terminating && orderToTask.isEmpty) {
      context.stop(self)
    }
  }

  override def toString = s"JobRunner(${jobPath.string})"

  private def taskCount = orderToTask.size
}

object JobRunner {
  def actorOf(jobPath: JobPath, newTaskRunner: TaskRunner.Factory)(implicit actorRefFactory: ActorRefFactory, ts: TimerService, ec: ExecutionContext): ActorRef =
    actorRefFactory.actorOf(
      Props { new JobRunner(jobPath, newTaskRunner) },
      name = toActorName(jobPath))

  def toActorName(o: JobPath): String =
    encodeAsActorName(o.withoutStartingSlash)

  def toJobPath(o: ActorPath): JobPath =
    JobPath("/" + decodeActorName(o.name))


  sealed trait Command
  object Command {
    final case class StartWithConfigurationFile(path: Path)
    final case class StartWithConfiguration(conf: JobConfiguration)
    final case class ReadConfigurationFile(path: Path)
    final case class ProcessOrder(order: Order[Order.InProcess.type], stdChannels: StdChannels) extends Command
  }

  object Response {
    case object Ready
    final case class OrderProcessed(orderId: OrderId, moduleStepEnded: TaskStepEnded)
  }

  object Input {
    case object OrderAvailable extends Command
    case object Terminate
  }

  object Output {
    case object ReadyForOrder
  }

  private object Internal {
    final case class TaskFinished(order: Order[Order.State], triedStepEnded: Try[TaskStepEnded])
    final case object KillAll
  }
}
