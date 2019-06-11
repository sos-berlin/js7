package com.sos.jobscheduler.agent.scheduler.job

import akka.actor.{Actor, DeadLetterSuppression, Props, Stash}
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.scheduler.job.JobActor._
import com.sos.jobscheduler.agent.scheduler.job.task.{TaskConfiguration, TaskRunner, TaskStepEnded, TaskStepFailed}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.jobscheduler.base.utils.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.process.Processes.ShellFileAttributes
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.system.OperatingSystem.{isUnix, isWindows}
import com.sos.jobscheduler.data.job.{ExecutablePath, ExecutableScript, JobKey}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.taskserver.task.process.RichProcess.tryDeleteFile
import com.sos.jobscheduler.taskserver.task.process.StdChannels
import java.nio.file.Files.{createTempFile, exists, getPosixFilePermissions}
import java.nio.file.LinkOption.NOFOLLOW_LINKS
import java.nio.file.Path
import java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE
import monix.execution.Scheduler
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
final class JobActor private(conf: Conf)(implicit scheduler: Scheduler)
extends Actor with Stash {
  import conf.{executablesDirectory, jobKey, newTaskRunner, temporaryDirectory, workflowJob}

  private val logger = Logger.withPrefix[JobActor](jobKey.keyName)
  private val orderToTask = mutable.Map[OrderId, Task]()
  private var waitingForNextOrder = false
  private var terminating = false
  private lazy val filePool = new FilePool(jobKey, temporaryDirectory)

  private val checkedExecutable: Checked[Executable] = workflowJob.executable match {
    case path: ExecutablePath =>
      val file = path.toFile(executablesDirectory)
      if (!exists(file)) {
        logger.warn(s"Executable '$file' is not accessible")
      } else if (isUnix && !Try(getPosixFilePermissions(file).contains(OWNER_EXECUTE)).getOrElse(true)) {
        logger.warn(s"Executable '$file' is not user executable")
      }
      Valid(Executable(file, path.string, false))

    case script: ExecutableScript =>
      if (!conf.scriptInjectionAllowed)
        Invalid(Problem.pure("Agent does not allow signed script jobs"))
      else
        Checked.catchNonFatal {
          val ext = if (isWindows) ".cmd" else ""
          val file = createTempFile(temporaryDirectory, "script-", ext, ShellFileAttributes: _*)
          file.write(script.string, AgentConfiguration.FileEncoding)
          Executable(file, "tmp/" + file.getFileName, true)
        }
  }

  checkedExecutable match {
    case Invalid(problem) => logger.error(problem.toString)
    case Valid(executable) => logger.debug(s"Ready - executable=$executable")
  }

  override def postStop() = {
    killAll(SIGKILL)
    filePool.close()
    for (o <- checkedExecutable) if (o.isTemporary) tryDeleteFile(o.file)
    super.postStop()
  }

  def receive = {
    case Input.OrderAvailable =>
      handleIfReadyForOrder()

    case cmd: Command.ProcessOrder if waitingForNextOrder =>
      if (cmd.jobKey != jobKey)
        sender() ! Response.OrderProcessed(cmd.order.id, TaskStepFailed(Problem.pure(s"Internal error: requested jobKey=${cmd.jobKey} ≠ JobActor's $jobKey")))
      else
        checkedExecutable match {
          case Invalid(problem) =>
            sender() ! Response.OrderProcessed(cmd.order.id, TaskStepFailed(problem))  // Exception.toString is published !!!
          case Valid(executable) =>
            if (!exists(executable.file)) {
              val msg = s"Executable '${executable.file}' is not accessible"
              logger.error(s"Order '${cmd.order.id.string}' step failed: $msg")
              sender() ! Response.OrderProcessed(cmd.order.id, TaskStepFailed(Problem.pure(msg)))
            } else
              Try(executable.file.toRealPath()) match {
                case Failure(t) =>
                  sender() ! Response.OrderProcessed(cmd.order.id, TaskStepFailed(Problem.pure(s"Executable '$executable': $t")))  // Exception.toString is published !!!
                case Success(executableFile) =>
                  assert(taskCount < workflowJob.taskLimit, "Task limit exceeded")
                  assert(executable.isTemporary || executableFile.startsWith(conf.executablesDirectory.toRealPath(NOFOLLOW_LINKS)),
                    s"Executable directory '${conf.executablesDirectory}' does not contain file '$executableFile' ")
                  val sender = this.sender()
                  processOrder(cmd, executableFile)
                    .onComplete { triedStepEnded =>
                      self.!(Internal.TaskFinished(cmd.order, triedStepEnded))(sender)
                    }
                  handleIfReadyForOrder()
              }
        }

    case Internal.TaskFinished(order, triedStepEnded) =>
      filePool.release(orderToTask(order.id).fileSet)
      orderToTask -= order.id
      sender() ! Response.OrderProcessed(order.id, recoverFromFailure(triedStepEnded))
      continueTermination()
      handleIfReadyForOrder()

    case Input.Terminate(sigtermProcesses, sigkillProcessesAfter) =>
      logger.debug("Terminate")
      terminating = true
      if (sigtermProcesses) {
        killAll(SIGTERM)
      }
      for (duration <- sigkillProcessesAfter if taskCount > 0) {
        scheduler.scheduleOnce(duration) {
          self ! Internal.KillAll
        }
      }
      continueTermination()

    case Input.KillProcess(orderId, sigtermProcesses, sigkillProcessesAfter) =>
      logger.debug(s"KillProcess $orderId")
      if (sigtermProcesses) {
        killOrder(orderId, SIGTERM)
      }
      for (duration <- sigkillProcessesAfter if taskCount > 0) {
        scheduler.scheduleOnce(duration) {
          self ! Internal.KillOrder(orderId)
        }
      }

    case Internal.KillAll =>
      killAll(SIGKILL)

    case Internal.KillOrder(orderId) =>
      killOrder(orderId, SIGKILL)
  }

  private def processOrder(cmd: Command.ProcessOrder, executableFile: Path): Future[TaskStepEnded] = {
    val fileSet = filePool.get()
    val taskRunner = newTaskRunner(TaskConfiguration(jobKey, workflowJob, executableFile, fileSet.shellReturnValuesProvider))
    orderToTask.insert(cmd.order.id -> Task(fileSet, taskRunner))
    val whenEnded = taskRunner.processOrder(cmd.order, cmd.defaultArguments, cmd.stdChannels)
      .andThen { case _ => taskRunner.terminate()/*for now (shell only), returns immediately s a completed Future*/ }
    waitingForNextOrder = false
    whenEnded
  }

  private def handleIfReadyForOrder(): Unit =
    if (!waitingForNextOrder && !terminating && taskCount < workflowJob.taskLimit) {
      context.parent ! Output.ReadyForOrder
      waitingForNextOrder = true
    }

  private def recoverFromFailure(tried: Try[TaskStepEnded]): TaskStepEnded =
    tried match {
      case Success(o) => o
      case Failure(t) =>
        logger.error(s"Job step failed: ${t.toStringWithCauses}", t)
        TaskStepFailed(Problem.pure(s"Job step failed: ${t.toStringWithCauses}"))  // Publish internal exception in event ???
    }

  private def killAll(signal: ProcessSignal): Unit =
    if (orderToTask.nonEmpty) {
      logger.warn(s"Terminating, sending $signal to all $taskCount tasks")
      for (orderId <- orderToTask.keys.toVector/*copy*/) {
        killOrder(orderId, signal)
      }
    }

  private def killOrder(orderId: OrderId, signal: ProcessSignal): Unit =
    for (task <- orderToTask.get(orderId)) {
      logger.warn(s"Kill $signal ${task.taskRunner.asBaseAgentTask.id} processing $orderId")
      task.taskRunner.kill(signal)
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
  def props(conf: Conf)(implicit s: Scheduler) = Props { new JobActor(conf) }

  final case class Conf(
    jobKey: JobKey,  // For integrity check
    workflowJob: WorkflowJob,
    newTaskRunner: TaskRunner.Factory,
    temporaryDirectory: Path,
    executablesDirectory: Path,
    scriptInjectionAllowed: Boolean)

  sealed trait Command
  object Command {
    final case class ProcessOrder(jobKey: JobKey, order: Order[Order.Processing], defaultArguments: Map[String, String], stdChannels: StdChannels)
    extends Command
  }

  object Response {
    final case class OrderProcessed(orderId: OrderId, taskStepEnded: TaskStepEnded)
  }

  object Input {
    case object OrderAvailable extends Command
    final case class Terminate(
      sigtermProcesses: Boolean = false,
      sigkillProcessesAfter: Option[FiniteDuration] = None)
    final case class KillProcess(
      orderId: OrderId,
      sigtermProcesses: Boolean = false,
      sigkillProcessesAfter: Option[FiniteDuration] = None)
  }

  object Output {
    case object ReadyForOrder
  }

  private object Internal {
    final case class TaskFinished(order: Order[Order.State], triedStepEnded: Try[TaskStepEnded])
    final case object KillAll extends DeadLetterSuppression
    final case class KillOrder(orderId: OrderId) extends DeadLetterSuppression
  }

  private case class Executable(file: Path, name: String, isTemporary: Boolean) {
    override def toString = name
  }

  private case class Task(fileSet: FilePool.FileSet, taskRunner: TaskRunner)
}
