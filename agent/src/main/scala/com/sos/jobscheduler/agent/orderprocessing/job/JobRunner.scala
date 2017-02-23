package com.sos.jobscheduler.agent.orderprocessing.job

import akka.actor.{Actor, ActorPath, ActorRef, ActorRefFactory, Props}
import com.sos.jobscheduler.agent.orderprocessing.job.JobRunner._
import com.sos.jobscheduler.agent.orderprocessing.job.task.TaskRunner
import com.sos.jobscheduler.agent.task.AgentTaskFactory
import com.sos.jobscheduler.common.akkautils.Akkas.{decodeActorName, encodeAsActorName}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.engine2.order.OrderEvent.{OrderStepEnded, OrderStepFailed, OrderStepSucceeded}
import com.sos.jobscheduler.data.engine2.order.{JobPath, Order}
import com.sos.jobscheduler.data.order.OrderId
import java.nio.file.Path
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

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
        .onComplete { tried ⇒
          val event = tried match {
            case Success(o: OrderStepSucceeded) ⇒ o
            case Failure(t) ⇒ OrderStepFailed(t.toString)  // We are exposing the exception message !!!
          }
          sender ! Response.OrderProcessed(order.id, event)
          self ! Internal.TaskFinished
        }

    case Internal.TaskFinished ⇒
      taskCount -= 1
      if (taskCount < jobConfiguration.taskLimit && !waitingForNextOrder) {
        context.parent ! Output.ReadyForOrder
        waitingForNextOrder = true
      }
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
    final case class OrderProcessed(orderId: OrderId, event: OrderStepEnded)
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
