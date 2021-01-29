package js7.agent.scheduler.job

import akka.actor.{Actor, DeadLetterSuppression, Props, Stash}
import cats.instances.vector._
import cats.syntax.traverse._
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
import js7.data.job.{AbsolutePathExecutable, CommandLine, CommandLineEvaluator, CommandLineExecutable, JobKey, RelativePathExecutable, ScriptExecutable}
import js7.data.order.{Order, OrderId}
import js7.data.value.expression.Evaluator
import js7.data.value.expression.Expression.ObjectExpression
import js7.data.value.{NamedValues, StringValue}
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

  private val checkedExecutable: Checked[(Order[Order.Processing], NamedValues, Workflow) => Checked[Execute]] =
    workflowJob.executable match {
      case AbsolutePathExecutable(path, envExpr, v1Compatible) =>
        if (!conf.scriptInjectionAllowed)
          Left(SignedInjectionNotAllowed)
        else {
          val file = Paths.get(path)
          warnIfNotExecutable(file)
          Right((order, executeArguments, workflow) =>
            evalEnv(toEvaluator(order, executeArguments, workflow), envExpr)
              .map(env => Execute(CommandLine.fromFile(file), path, env, v1Compatible = v1Compatible)))
        }

      case ex @ RelativePathExecutable(path, envExpr, v1Compatible) =>
        Right { (order, executeArguments, workflow) =>
          val file = ex.toFile(executablesDirectory)
          warnIfNotExecutable(file)
          evalEnv(toEvaluator(order, executeArguments, workflow), envExpr)
            .map(env => Execute(CommandLine.fromFile(file), path, env, v1Compatible = v1Compatible))
        }

      case ex @ CommandLineExecutable(commandLineExpression, envExpr) =>
        Right { (order, executeArguments, workflow) =>
          val evaluator = toEvaluator(order, executeArguments, workflow)
          new CommandLineEvaluator(evaluator)
            .eval(commandLineExpression)
            .flatMap { commandLine =>
              warnIfNotExecutable(commandLine.file)
              evalEnv(evaluator, envExpr)
                .map(env =>
                  Execute(commandLine, name = commandLine.file.getFileName.toString, env, v1Compatible = ex.v1Compatible))
            }
        }

      case ScriptExecutable(script, envExpr, v1Compatible) =>
        if (!conf.scriptInjectionAllowed)
          Left(SignedInjectionNotAllowed)
        else {
          val file = createTempFile(temporaryDirectory, "script-", isWindows ?? ".cmd", ShellFileAttributes: _*)
          Checked.catchNonFatal {
            file.write(script, AgentConfiguration.FileEncoding)
          }.map { _ =>
            temporaryFile := file
            (order, executeArguments, workflow) =>
              evalEnv(toEvaluator(order, executeArguments, workflow), envExpr).map(env =>
                Execute(CommandLine.fromFile(file), name = "tmp/" + file.getFileName,
                  env, v1Compatible = v1Compatible, isTemporary = true))
          }
        }
    }

  private def toEvaluator(order: Order[Order.Processing], defaultArguments: NamedValues, workflow: Workflow): Evaluator =
    new Evaluator(OrderContext.makeScope(Map.empty, order, workflow,
      default = defaultArguments orElse conf.workflowJob.defaultArguments))

  private def evalEnv(evaluator: Evaluator, envExpr: ObjectExpression): Checked[Map[String, String]] =
    evaluator.evalObjectExpression(envExpr)
      .flatMap(_.nameToValue.toVector.traverse { case (k, v) => v.toStringValue.map(k -> _.string) })
      .map(_.toMap)

  private def warnIfNotExecutable(file: Path): Unit =
    if (!exists(file)) {
      logger.warn(s"Executable '$file' not found")
    } else if (isUnix && !Try(getPosixFilePermissions(file) contains OWNER_EXECUTE).getOrElse(true)) {
      logger.warn(s"Executable '$file' is not user executable")
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
      waitingForNextOrder = false
      if (cmd.jobKey != jobKey) {
        replyWithOrderProcessed(Response.OrderProcessed(
          cmd.order.id,
          TaskStepFailed(Problem.pure(s"Internal error: requested jobKey=${cmd.jobKey} ≠ JobActor's $jobKey")), isKilled = false))
      } else {
        val killed = orderToRun.get(cmd.order.id).fold(false)(_.isKilled)
        checkedExecutable.flatMap(_(cmd.order, cmd.defaultArguments, cmd.workflow)) match {
          case Left(problem) =>
            replyWithOrderProcessed(
              Response.OrderProcessed(cmd.order.id, TaskStepFailed(problem), killed))  // Exception.toString is published !!!
          case Right(execute) =>
            if (!exists(execute.file)) {
              val msg = s"Execute '${execute.file}' not found"
              logger.error(s"Order '${cmd.order.id.string}' step failed: $msg")
              replyWithOrderProcessed(
                Response.OrderProcessed(cmd.order.id, TaskStepFailed(Problem.pure(msg)), killed))
            } else
              Try(execute.file.toRealPath()) match {
                case Failure(t) =>
                  replyWithOrderProcessed(
                    Response.OrderProcessed(cmd.order.id, TaskStepFailed(Problem.pure(s"Execute '$execute': $t")), killed))  // Exception.toString is published !!!
                case Success(executableFile) =>
                  assertThat(taskCount < workflowJob.taskLimit, "Task limit exceeded")
                  if (!execute.isTemporary && !conf.scriptInjectionAllowed) {
                    assertThat(execute.isTemporary || executableFile.startsWith(executablesDirectory),
                      s"Execute directory '${conf.executablesDirectory}' does not contain file '$executableFile' ")
                  }
                  val v1Env =
                    if (!execute.v1Compatible) Map.empty[String, String]
                    else
                      (conf.workflowJob.defaultArguments ++ cmd.defaultArguments ++ cmd.order.namedValues)
                        .view
                        .mapValues(_.toStringValue)
                        .collect {
                          case (name, Right(v)) => name -> v  // ignore toStringValue errors (ListValue)
                        }
                        .map { case (k, StringValue(v)) => (DefaultV1EnvPrefix + k.toUpperCase) -> v }
                        .toMap

                  val sender = this.sender()
                  processOrder(cmd.order.id, execute.commandLine, v1Env ++ execute.env, cmd.stdChannels)
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
      replyWithOrderProcessed(
        Response.OrderProcessed(order.id, recoverFromFailure(triedStepEnded), isKilled = run.isKilled))

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

  private def replyWithOrderProcessed(msg: Response.OrderProcessed): Unit = {
    sender() ! msg
    continueTermination()
    handleIfReadyForOrder()
  }

  private def processOrder(
    orderId: OrderId,
    commandLine: CommandLine,
    env: Map[String, String],
    stdChannels: StdChannels)
  : Future[TaskStepEnded] = {
    val taskRunner = newTaskRunner(TaskConfiguration(jobKey, workflowJob, commandLine))
    orderToRun.insert(orderId -> Run(taskRunner))
    taskRunner.processOrder(orderId, env, stdChannels)
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
      logger.info(s"Kill $signal ${taskRunner.asBaseAgentTask.id} processing $orderId")
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
  private val DefaultV1EnvPrefix = "SCHEDULER_PARAM_"

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

  private case class Execute(
    commandLine: CommandLine,
    name: String,
    env: Map[String, String],
    v1Compatible: Boolean,
    isTemporary: Boolean = false)
  {
    def file = commandLine.file
    override def toString = name
  }

  private final case class Run(taskRunner: TaskRunner, var isKilled: Boolean = false)
}
