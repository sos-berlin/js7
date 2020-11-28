package js7.agent.scheduler.job

import akka.actor.{Actor, DeadLetterSuppression, Props, Stash}
import java.nio.file.Files.{createTempFile, exists, getPosixFilePermissions}
import java.nio.file.LinkOption.NOFOLLOW_LINKS
import java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE
import java.nio.file.{Path, Paths}
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.Problems.SignedInjectionNotAllowed
import js7.agent.scheduler.job.JobActor._
import js7.agent.scheduler.job.task.{TaskConfiguration, TaskRunner, TaskStepEnded, TaskStepFailed}
import js7.base.problem.{Checked, Problem}
import js7.base.process.ProcessSignal
import js7.base.process.ProcessSignal.SIGKILL
import js7.base.system.OperatingSystem.{isUnix, isWindows}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Collections.implicits.InsertableMutableMap
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.SetOnce
import js7.common.process.Processes.ShellFileAttributes
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Logger
import js7.data.execution.workflow.context.OrderContext
import js7.data.job.{AbsoluteExecutablePath, CommandLine, CommandLineEvaluator, CommandLineExecutable, ExecutableScript, JobKey, RelativeExecutablePath}
import js7.data.order.{Order, OrderId}
import js7.data.value.NamedValues
import js7.data.value.expression.Evaluator
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.taskserver.task.process.RichProcess.tryDeleteFile
import js7.taskserver.task.process.StdChannels
import monix.execution.Scheduler
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
final class JobActor private(conf: Conf)(implicit scheduler: Scheduler)
extends Actor with Stash
{
  import conf.{jobKey, newTaskRunner, temporaryDirectory, workflowJob}

  private val logger = Logger.withPrefix[this.type](jobKey.keyName)
  private val executablesDirectory = conf.executablesDirectory.toRealPath(NOFOLLOW_LINKS)
  private val orderToRun = mutable.Map[OrderId, Run]()
  private var waitingForNextOrder = false
  private var terminating = false
  private var temporaryFile = SetOnce[Path]

  private val checkedExecutable: Checked[(Order[Order.Processing], Workflow) => Checked[Executable]] =
    workflowJob.executable match {
      case AbsoluteExecutablePath(path) =>
        if (!conf.scriptInjectionAllowed)
          Left(SignedInjectionNotAllowed)
        else
          Right((_, _) => toExecutable(Paths.get(path), path))

      case path: RelativeExecutablePath =>
        Right((_, _) => toExecutable(path.toFile(executablesDirectory), path.string))

      case CommandLineExecutable(commandLineExpression) =>
        Right { (order, workflow) =>
          new CommandLineEvaluator(new Evaluator(OrderContext.makeScope(order, workflow)))
            .eval(commandLineExpression)
            .map(commandLine => Executable(commandLine, name = commandLine.file.getFileName.toString))
        }

      case ExecutableScript(script) =>
        if (!conf.scriptInjectionAllowed)
          Left(SignedInjectionNotAllowed)
        else {
          val file = createTempFile(temporaryDirectory, "script-", isWindows ?? ".cmd", ShellFileAttributes: _*)
          Checked.catchNonFatal {
            file.write(script, AgentConfiguration.FileEncoding)
            Executable(CommandLine.fromFile(file), name = "tmp/" + file.getFileName, isTemporary = true)
          }.map { _ =>
            temporaryFile := file
            (_, _) => Right(Executable(CommandLine.fromFile(file), name = "tmp/" + file.getFileName, isTemporary = true))
          }
        }
    }

  private def toExecutable(file: Path, name: String): Checked[Executable] = {
    if (!exists(file)) {
      logger.warn(s"Executable '$file' not found")
    } else if (isUnix && !Try(getPosixFilePermissions(file) contains OWNER_EXECUTE).getOrElse(true)) {
      logger.warn(s"Executable '$file' is not user executable")
    }
    Right(Executable(CommandLine.fromFile(file), name))
  }

  for (problem <- checkedExecutable.left) logger.error(problem.toString)

  override def postStop() = {
    killAll(SIGKILL)
    temporaryFile foreach tryDeleteFile
    super.postStop()
  }

  def receive = {
    case Input.OrderAvailable =>
      handleIfReadyForOrder()

    case cmd: Command.ProcessOrder if waitingForNextOrder =>
      if (cmd.jobKey != jobKey)
        sender() ! Response.OrderProcessed(
          cmd.order.id,
          TaskStepFailed(Problem.pure(s"Internal error: requested jobKey=${cmd.jobKey} ≠ JobActor's $jobKey")), isKilled = false)
      else {
        val killed = orderToRun.get(cmd.order.id).fold(false)(_.isKilled)
        checkedExecutable.flatMap(_(cmd.order, cmd.workflow)) match {
          case Left(problem) =>
            sender() ! Response.OrderProcessed(cmd.order.id, TaskStepFailed(problem), killed)  // Exception.toString is published !!!
          case Right(executable) =>
            if (!exists(executable.file)) {
              val msg = s"Executable '${executable.file}' not found"
              logger.error(s"Order '${cmd.order.id.string}' step failed: $msg")
              sender() ! Response.OrderProcessed(cmd.order.id, TaskStepFailed(Problem.pure(msg)), killed)
            } else
              Try(executable.file.toRealPath()) match {
                case Failure(t) =>
                  sender() ! Response.OrderProcessed(cmd.order.id, TaskStepFailed(Problem.pure(s"Executable '$executable': $t")), killed)  // Exception.toString is published !!!
                case Success(executableFile) =>
                  assertThat(taskCount < workflowJob.taskLimit, "Task limit exceeded")
                  if (!executable.isTemporary && !conf.scriptInjectionAllowed) {
                    assertThat(executable.isTemporary || executableFile.startsWith(executablesDirectory),
                      s"Executable directory '${conf.executablesDirectory}' does not contain file '$executableFile' ")
                  }
                  val sender = this.sender()
                  processOrder(cmd, executable.commandLine)
                    .onComplete { triedStepEnded =>
                      self.!(Internal.TaskFinished(cmd.order, triedStepEnded))(sender)
                    }
                  handleIfReadyForOrder()
              }
        }
      }

    case Internal.TaskFinished(order, triedStepEnded) =>
      val run = orderToRun(order.id)
      orderToRun -= order.id
      sender() ! Response.OrderProcessed(order.id, recoverFromFailure(triedStepEnded), isKilled = run.isKilled)
      continueTermination()
      handleIfReadyForOrder()

    case Input.Terminate(maybeSignal) =>
      logger.debug("Terminate")
      terminating = true
      for (signal <- maybeSignal) {
        killAll(signal)
      }
      if (maybeSignal != Some(SIGKILL) && taskCount > 0) {
        scheduler.scheduleOnce(conf.sigkillProcessesAfter) {
          self ! Internal.KillAll
        }
      }
      continueTermination()

    case Input.KillProcess(orderId, maybeSignal) =>
      logger.debug(s"KillProcess $orderId")
      for (signal <- maybeSignal) {
        killOrder(orderId, signal)
      }
      if (maybeSignal != Some(SIGKILL) && taskCount > 0) {
        scheduler.scheduleOnce(conf.sigkillProcessesAfter) {
          self ! Internal.KillOrder(orderId)
        }
      }

    case Internal.KillAll =>
      killAll(SIGKILL)

    case Internal.KillOrder(orderId) =>
      killOrder(orderId, SIGKILL)
  }

  private def processOrder(cmd: Command.ProcessOrder, commandLine: CommandLine): Future[TaskStepEnded] = {
    waitingForNextOrder = false
    val taskRunner = newTaskRunner(TaskConfiguration(jobKey, workflowJob, commandLine))
    orderToRun.insert(cmd.order.id -> Run(taskRunner))
    taskRunner.processOrder(cmd.order, cmd.defaultArguments, cmd.stdChannels)
      .guarantee(taskRunner.terminate/*for now (shell only), returns immediately s a completed Future*/)
      .runToFuture
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
    if (orderToRun.nonEmpty) {
      logger.warn(s"Terminating, sending $signal to all $taskCount tasks")
      for (orderId <- orderToRun.keys.toVector/*copy*/) {
        killOrder(orderId, signal)
      }
    }

  private def killOrder(orderId: OrderId, signal: ProcessSignal): Unit =
    for (run <- orderToRun.get(orderId)) {
      import run.taskRunner
      logger.warn(s"Kill $signal ${taskRunner.asBaseAgentTask.id} processing $orderId")
      run.isKilled = true
      taskRunner.kill(signal)
    }

  private def continueTermination(): Unit =
    if (terminating) {
      if (orderToRun.isEmpty) {
        context.stop(self)
      } else {
        logger.debug(s"Awaiting termination of ${orderToRun.size} tasks")
      }
    }

  override def toString = s"JobActor(${jobKey.toString})"

  private def taskCount = orderToRun.size
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
    sigkillProcessesAfter: FiniteDuration,
    scriptInjectionAllowed: Boolean)

  sealed trait Command
  object Command {
    final case class ProcessOrder(
      jobKey: JobKey,
      order: Order[Order.Processing],
      workflow: Workflow,
      defaultArguments: NamedValues,
      stdChannels: StdChannels)
    extends Command
  }

  object Response {
    final case class OrderProcessed(orderId: OrderId, taskStepEnded: TaskStepEnded, isKilled: Boolean)
  }

  object Input {
    case object OrderAvailable extends Command
    final case class Terminate(signal: Option[ProcessSignal] = None)
    final case class KillProcess(orderId: OrderId, signal: Option[ProcessSignal])
  }

  object Output {
    case object ReadyForOrder
  }

  private object Internal {
    final case class TaskFinished(order: Order[Order.State], triedStepEnded: Try[TaskStepEnded])
    final case object KillAll extends DeadLetterSuppression
    final case class KillOrder(orderId: OrderId) extends DeadLetterSuppression
  }

  private case class Executable(commandLine: CommandLine, name: String, isTemporary: Boolean = false)
  {
    def file = commandLine.file
    override def toString = name
  }

  private final case class Run(taskRunner: TaskRunner, var isKilled: Boolean = false)
}
