package com.sos.jobscheduler.agent.scheduler.job

import akka.actor.{Actor, DeadLetterSuppression, Props, Stash}
import akka.pattern.pipe
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.scheduler.job.JobActor._
import com.sos.jobscheduler.agent.scheduler.job.task.{TaskConfiguration, TaskRunner, TaskStepEnded, TaskStepFailed}
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.jobscheduler.base.utils.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.process.Processes.newTemporaryShellFile
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.SideEffect.ImplicitSideEffect
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.jobnet.JobPath
import com.sos.jobscheduler.data.order.Order.Bad
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.taskserver.data.TaskServerConfiguration.Encoding
import com.sos.jobscheduler.taskserver.task.process.RichProcess.tryDeleteFile
import com.sos.jobscheduler.taskserver.task.process.StdChannels
import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.file.Path
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
final class JobActor private(jobPath: JobPath, newTaskRunner: TaskRunner.Factory, timerService: TimerService)(implicit ec: ExecutionContext)
extends Actor with Stash {

  private val logger = Logger.withPrefix[JobActor](jobPath.toString)
  private val orderToTask = mutable.Map[OrderId, Entry]()
  private var waitingForNextOrder = false
  private var terminating = false
  private var jobConfiguration: JobConfiguration = null
  private lazy val filePool = new FilePool(jobConfiguration)
  private lazy val shellFile = newTemporaryShellFile(jobConfiguration.path.name) sideEffect { file ⇒
    autoClosing(new OutputStreamWriter(new FileOutputStream(file), Encoding)) { w ⇒
      val content = jobConfiguration.script.string.trim
      if (content.nonEmpty) {
        w.write(jobConfiguration.script.string.trim)
        w.write("\n")
      }
    }
  }

  override def postStop() = {
    killAll(SIGKILL)
    if (shellFile != null) {
      tryDeleteFile(shellFile)
    }
    super.postStop()
  }

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

    case cmd: Command.ProcessOrder if waitingForNextOrder ⇒
      logger.trace(s"ProcessOrder(${cmd.order.id})")
      assert(taskCount < jobConfiguration.taskLimit, "Task limit exceeded")
      val fileSet = filePool.get()
      newTaskRunner(TaskConfiguration(jobConfiguration, shellFile, fileSet.shellReturnValuesProvider))
        .map(runner ⇒ Internal.TaskRegistered(cmd, fileSet, runner))
        .pipeTo(self)(sender())

    case Internal.TaskRegistered(Command.ProcessOrder(order, stdoutStderrWriter), fileSet, taskRunner) ⇒
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
      handleStop()
      handleIfReadyForOrder()

    case AgentCommand.Terminate(sigtermProcesses, sigkillProcessesAfter) ⇒
      logger.debug("Terminating")
      terminating = true
      if (sigtermProcesses) {
        killAll(SIGTERM)
      }
      sigkillProcessesAfter match {
        case Some(duration) if taskCount > 0 ⇒
          timerService.delayedFuture(duration) {
            self ! Internal.KillAll
          }
        case _ ⇒
      }
      handleStop()

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
        logger.error(s"TaskRunner.stepOne failed: ${t.toStringWithCauses}", t)
        TaskStepFailed(Bad("TaskRunner.stepOne failed"))
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

  private def handleStop(): Unit = {
    if (terminating) {
      if (orderToTask.isEmpty) {
        context.stop(self)
      } else {
        logger.debug(s"Still awaiting termination of ${orderToTask.size} tasks")
      }
    }
  }

  override def toString = s"JobActor(${jobPath.string})"

  private def taskCount = orderToTask.size
}

object JobActor {
  def props(jobPath: JobPath, newTaskRunner: TaskRunner.Factory, ts: TimerService)(implicit ec: ExecutionContext): Props =
    Props { new JobActor(jobPath, newTaskRunner, ts) }

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
    final case class TaskRegistered(processOrder: Command.ProcessOrder, fileSet: FilePool.FileSet, taskRunner: TaskRunner)
    final case class TaskFinished(order: Order[Order.State], triedStepEnded: Try[TaskStepEnded])
    final case object KillAll extends DeadLetterSuppression
  }

  private case class Entry(fileSet: FilePool.FileSet, taskRunner: TaskRunner)
}
