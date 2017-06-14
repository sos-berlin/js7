package com.sos.jobscheduler.agent.scheduler.job

import akka.actor.{Actor, ActorPath, ActorRef, ActorRefFactory, Props}
import com.sos.jobscheduler.agent.scheduler.job.JobRunner._
import com.sos.jobscheduler.agent.scheduler.job.task.ModuleInstanceRunner.{ModuleStepEnded, ModuleStepFailed}
import com.sos.jobscheduler.agent.scheduler.job.task.TaskRunner
import com.sos.jobscheduler.agent.task.AgentTaskFactory
import com.sos.jobscheduler.common.akkautils.Akkas.{decodeActorName, encodeAsActorName}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.jobnet.JobPath
import com.sos.jobscheduler.data.order.Order.Bad
import com.sos.jobscheduler.data.order.{Order, OrderId}
import java.nio.file.Path
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
final class JobRunner private(jobPath: JobPath)(implicit newTask: AgentTaskFactory, ec: ExecutionContext)
extends Actor {

  private val logger = Logger.withPrefix[JobRunner](jobPath.string)
  private var taskCount = 0
  private var waitingForNextOrder = false

  def receive = {
    case Command.ReadConfigurationFile(path: Path) ⇒
      val conf = JobConfiguration.parseXml(jobPath, path)  // TODO May crash
      context.become(started(conf))
      sender() ! Response.Started
  }

  private def started(jobConfiguration: JobConfiguration): Receive = {
    case Input.OrderAvailable ⇒
      if (taskCount < jobConfiguration.taskLimit && !waitingForNextOrder) {
        context.parent ! Output.ReadyForOrder
        waitingForNextOrder = true
      }

    case Command.ProcessOrder(order) if waitingForNextOrder ⇒
      logger.trace(s"ProcessOrder(${order.id})")
      waitingForNextOrder = false
      taskCount += 1
      if (taskCount < jobConfiguration.taskLimit) {
        context.parent ! Output.ReadyForOrder
        waitingForNextOrder = true
      }
      val sender = this.sender()
      TaskRunner.stepOne(jobConfiguration, order)
        .onComplete { triedStepEnded ⇒
          sender ! Response.OrderProcessed(order.id, recoverFromFailure(triedStepEnded))
          self ! Internal.TaskFinished
        }

    case Internal.TaskFinished ⇒
      taskCount -= 1
      if (taskCount < jobConfiguration.taskLimit && !waitingForNextOrder) {
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

  override def toString = s"JobRunner(${jobPath.string})"
}

object JobRunner {
  def actorOf(jobPath: JobPath)(implicit actorRefFactory: ActorRefFactory, newTask: AgentTaskFactory, ec: ExecutionContext): ActorRef =
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
    case object Started
    final case class OrderProcessed(orderId: OrderId, moduleStepEnded: ModuleStepEnded)
  }

  object Input {
    case object OrderAvailable extends Command
  }

  object Output {
    case object ReadyForOrder
  }

  private object Internal {
    final case object TaskFinished
  }
}
