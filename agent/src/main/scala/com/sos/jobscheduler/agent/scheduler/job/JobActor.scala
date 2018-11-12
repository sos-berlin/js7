package com.sos.jobscheduler.agent.scheduler.job

import akka.actor.{Actor, DeadLetterSuppression, Props, Stash}
import akka.pattern.pipe
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.scheduler.job.JobActor._
import com.sos.jobscheduler.agent.scheduler.job.task.{TaskConfiguration, TaskRunner, TaskStepEnded, TaskStepFailed}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.jobscheduler.base.utils.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.job.JobKey
import com.sos.jobscheduler.data.order.Outcome.Disrupted
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.taskserver.task.process.StdChannels
import java.nio.file.Files.exists
import java.nio.file.Path
import monix.execution.Scheduler
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
final class JobActor private(jobKey: JobKey, workflowJob: WorkflowJob, newTaskRunner: TaskRunner.Factory,
  executableDirectory: Path)
  (implicit scheduler: Scheduler)
extends Actor with Stash {

  private val logger = Logger.withPrefix[JobActor](jobKey.toString)
  private val orderToTask = mutable.Map[OrderId, Entry]()
  private var waitingForNextOrder = false
  private var terminating = false
  private lazy val filePool = new FilePool(jobKey, workflowJob)
  private val uncheckedFile = workflowJob.executablePath.toFile(executableDirectory)

  if (!exists(uncheckedFile)) {
    logger.warn(s"Executable '${workflowJob.executablePath}' is not accessible")
  }

  override def preStart(): Unit = {
    super.preStart()
    logger.debug(s"Ready - executable=${workflowJob.executablePath}")
  }

  override def postStop() = {
    killAll(SIGKILL)
    filePool.close()
    super.postStop()
  }

  def receive = {
    case Input.OrderAvailable ⇒
      handleIfReadyForOrder()

    case cmd: Command.ProcessOrder if waitingForNextOrder ⇒
      logger.debug(s"ProcessOrder(${cmd.order.id})")
      if (cmd.jobKey != jobKey)
        sender() ! Response.OrderProcessed(cmd.order.id, TaskStepFailed(Disrupted(Problem.eager(s"Internal error: requested jobKey=${cmd.jobKey} ≠ JobActor's $jobKey"))))
      else {
        assert(taskCount < workflowJob.taskLimit, "Task limit exceeded")
        if (!exists(uncheckedFile)) {
          val msg = s"Executable '${workflowJob.executablePath}' is not accessible"
          logger.error(s"Order '${cmd.order.id.string}' step failed: $msg")
          sender() ! Response.OrderProcessed(cmd.order.id, TaskStepFailed(Disrupted(Problem.eager(msg))))
        } else {
          Try(uncheckedFile.toRealPath()) match {
            case Failure(t) ⇒
              sender() ! Response.OrderProcessed(cmd.order.id, TaskStepFailed(Disrupted(Problem.eager(s"Executable '${workflowJob.executablePath}': $t"))))  // Exception.toString is published !!!
            case Success(executableFile) ⇒
              assert(executableFile startsWith executableDirectory.toRealPath(), s"Executable directory '$executableDirectory' does not contain file '$executableFile' ")
              val fileSet = filePool.get()
              newTaskRunner(TaskConfiguration(jobKey, workflowJob, executableFile, fileSet.shellReturnValuesProvider))
                .map(runner ⇒ Internal.TaskRegistered(cmd, fileSet, runner))
                .pipeTo(self)(sender())
          }
        }
      }

    case Internal.TaskRegistered(Command.ProcessOrder(_, order, stdoutStderrWriter), fileSet, taskRunner) ⇒
      if (terminating) {
        taskRunner.kill(SIGKILL)  // Kill before start
      } else {
        orderToTask.insert(order.id → Entry(fileSet, taskRunner))
        val sender = this.sender()
        taskRunner.processOrder(order, stdoutStderrWriter)
          .andThen { case _ ⇒ taskRunner.terminate()/*for now (shell only), returns immediately s a completed Future*/ }
          .onComplete { triedStepEnded ⇒
            self.!(Internal.TaskFinished(order, triedStepEnded))(sender)
          }
        waitingForNextOrder = false
        handleIfReadyForOrder()
      }

    case Internal.TaskFinished(order, triedStepEnded) ⇒
      filePool.release(orderToTask(order.id).fileSet)
      orderToTask -= order.id
      sender() ! Response.OrderProcessed(order.id, recoverFromFailure(triedStepEnded))
      continueTermination()
      handleIfReadyForOrder()

    case AgentCommand.Terminate(sigtermProcesses, sigkillProcessesAfter) ⇒
      logger.debug("Terminate")
      terminating = true
      if (sigtermProcesses) {
        killAll(SIGTERM)
      }
      sigkillProcessesAfter match {
        case Some(duration) if taskCount > 0 ⇒
          scheduler.scheduleOnce(duration) {
            self ! Internal.KillAll
          }
        case _ ⇒
      }
      continueTermination()

    case Internal.KillAll ⇒
      killAll(SIGKILL)
  }

  private def handleIfReadyForOrder() =
    if (!waitingForNextOrder && !terminating && taskCount < workflowJob.taskLimit) {
      context.parent ! Output.ReadyForOrder
      waitingForNextOrder = true
    }

  private def recoverFromFailure(tried: Try[TaskStepEnded]): TaskStepEnded =
    tried match {
      case Success(o) ⇒ o
      case Failure(t) ⇒
        logger.error(s"Job step failed: ${t.toStringWithCauses}", t)
        TaskStepFailed(Disrupted(Problem.eager(s"Job step failed: ${t.toStringWithCauses}")))  // Publish internal exception in event ???
    }

  private def killAll(signal: ProcessSignal): Unit = {
    if (orderToTask.nonEmpty) {
      logger.warn(s"Terminating, sending $signal to all $taskCount tasks")
      for ((orderId, t) ← orderToTask) {
        logger.warn(s"Kill $signal ${t.taskRunner.asBaseAgentTask.id} processing $orderId")
        t.taskRunner.kill(signal)
      }
    }
  }

  private def continueTermination(): Unit =
    if (terminating) {
      if (orderToTask.isEmpty) {
        context.stop(self)
      } else {
        logger.debug(s"Awaiting termination of ${orderToTask.size} tasks")
      }
    }

  override def toString = s"JobActor(${jobKey.toString})"

  private def taskCount = orderToTask.size
}

object JobActor
{
  /** @param jobKey for integrity check
    */
  def props(jobKey: JobKey, workflowJob: WorkflowJob, newTaskRunner: TaskRunner.Factory, executableDirectory: Path)
    (implicit s: Scheduler)
  = Props { new JobActor(jobKey, workflowJob, newTaskRunner, executableDirectory) }

  sealed trait Command
  object Command {
    final case class ProcessOrder(jobKey: JobKey, order: Order[Order.InProcess], stdChannels: StdChannels) extends Command
  }

  object Response {
    final case class OrderProcessed(orderId: OrderId, taskStepEnded: TaskStepEnded)
  }

  object Input {
    case object OrderAvailable extends Command
    case object Terminate
  }

  object Output {
    case object ReadyForOrder
  }

  private object Internal {
    final case class TaskRegistered(processOrder: Command.ProcessOrder, fileSet: FilePool.FileSet, taskRunner: TaskRunner)
    final case class TaskFinished(order: Order[Order.State], triedStepEnded: Try[TaskStepEnded])
    final case object KillAll extends DeadLetterSuppression
  }

  private case class Entry(fileSet: FilePool.FileSet, taskRunner: TaskRunner)
}
