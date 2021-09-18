package js7.executor.process

import cats.syntax.traverse._
import js7.base.generic.Completed
import js7.base.io.process.ProcessSignal
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.system.OperatingSystem.isWindows
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{SetOnce, TaskLock}
import js7.data.job.TaskId.newGenerator
import js7.data.job.{ProcessExecutable, TaskId}
import js7.data.order.{OrderId, Outcome}
import js7.data.value.NamedValues
import js7.executor.StdObservers
import js7.executor.configuration.{JobExecutorConf, TaskConfiguration}
import js7.executor.forwindows.{WindowsLogon, WindowsProcess}
import js7.executor.process.ProcessDriver._
import js7.executor.process.ShellScriptProcess.startPipedShellScript
import monix.eval.{Fiber, Task}
import scala.concurrent.Promise
import scala.util.{Failure, Success}

final class ProcessDriver(
  orderId: OrderId,
  conf: TaskConfiguration,
  jobExecutorConf: JobExecutorConf)
{
  import jobExecutorConf.iox

  private val taskId = taskIdGenerator.next()
  private val checkedWindowsLogon = conf.login.traverse(WindowsLogon.fromKeyLogin)
  private lazy val returnValuesProvider = new ShellReturnValuesProvider(
    jobExecutorConf.temporaryDirectory,
    v1Compatible = conf.v1Compatible)
  private val terminatedPromise = Promise[Completed]()
  private val richProcessOnce = SetOnce[RichProcess]
  private val startProcessLock = TaskLock(orderId.toString)
  @volatile private var killedBeforeStart: Option[ProcessSignal] = None

  def startAndRunProcess(env: Map[String, String], stdObservers: StdObservers)
  : Task[Fiber[Outcome.Completed]] =
    startProcess(env, stdObservers)
      .flatMap {
        case Left(problem) => Task.pure(Outcome.Failed.fromProblem(problem): Outcome.Completed).start
        case Right(richProcess) => outcomeOf(richProcess).start
      }

  private def startProcess(env: Map[String, String], stdObservers: StdObservers)
  : Task[Checked[RichProcess]] =
    Task.deferAction { implicit scheduler =>
      killedBeforeStart match {
        case Some(signal) =>
          Task.pure(Left(Problem.pure(s"Processing killed before start with $signal")))

        case None =>
          Task(
            checkedWindowsLogon
              .flatMap { maybeWindowsLogon =>
                Checked.catchNonFatal {
                  for (o <- maybeWindowsLogon)
                    WindowsProcess.makeFileAppendableForUser(returnValuesProvider.file, o.userName)
                }
              .map(_ =>
                ProcessConfiguration(
                  workingDirectory = Some(jobExecutorConf.workingDirectory),
                  additionalEnvironment = env + returnValuesProvider.toEnv,
                  maybeTaskId = Some(taskId),
                  killScriptOption = jobExecutorConf.killScript,
                  maybeWindowsLogon))
            }
          ).flatMapT(processConfiguration =>
            startProcessLock.lock("startProcess")(
              globalStartProcessLock.lock(orderId.toString)(
                startPipedShellScript(conf.commandLine, processConfiguration, stdObservers))
                .flatMapT { richProcess =>
                  logger.info(s"$orderId: Process $richProcess started, ${conf.jobKey}: ${conf.commandLine}")
                  terminatedPromise.future.value match {
                    case Some(Failure(t)) => Task.pure(Left(Problem.fromThrowable(t)))
                    case Some(Success(_)) => Task.pure(Left(Problem("Duplicate process start?")))
                    case None =>
                      terminatedPromise.completeWith(
                        richProcess.terminated.as(Completed).runToFuture)
                      richProcessOnce := richProcess
                      killedBeforeStart
                        .traverse(sendProcessSignal(richProcess, _))
                        .as(Right(richProcess))
                  }
                }))
      }
    }

  private def outcomeOf(richProcess: RichProcess): Task[Outcome.Completed] =
    richProcess
      .terminated
      .materialize.flatMap { tried =>
        val rc = tried.map(_.pretty(isWindows = isWindows)).getOrElse(tried)
        logger.info(
          s"$orderId: Process $richProcess terminated with $rc after ${richProcess.duration.pretty}")
        Task.fromTry(tried)
      }
      .map { returnCode =>
        fetchReturnValuesThenDeleteFile() match {
          case Left(problem) =>
            Outcome.Failed.fromProblem(
              problem.withPrefix("Reading return values failed:"),
              Map(ProcessExecutable.toNamedValue(returnCode)))

          case Right(namedValues) =>
            conf.toOutcome(namedValues, returnCode)
        }
      }
      .guarantee(Task {
        returnValuesProvider.tryDeleteFile()
      })

  private def fetchReturnValuesThenDeleteFile(): Checked[NamedValues] =
    Checked.catchNonFatal {
      val result = returnValuesProvider.read()
      returnValuesProvider.tryDeleteFile()
      result
    }

  def kill(signal: ProcessSignal): Task[Unit] =
    startProcessLock.lock("kill")(Task.defer {
      richProcessOnce.toOption match {
        case None =>
          logger.debug(s"$orderId: Kill before start")
          Task {
            terminatedPromise.tryFailure(new RuntimeException(
              s"$taskId killed before start with $signal"))
            killedBeforeStart = Some(signal)
          }
        case Some(richProcess) =>
          sendProcessSignal(richProcess, signal)
      }
    })

  private def sendProcessSignal(richProcess: RichProcess, signal: ProcessSignal): Task[Unit] =
    Task.defer {
      logger.info(s"$orderId: Process $richProcess: kill \"$signal\"")
      richProcess.sendProcessSignal(signal)
    }

  override def toString = s"ProcessDriver($taskId ${conf.jobKey})"
}

object ProcessDriver
{
  private val logger = Logger(getClass)

  /** Linux may return a "busy" error when starting many processes at once. */
  private val globalStartProcessLock = TaskLock("globalStartProcessLock")

  private object taskIdGenerator extends Iterator[TaskId] {
    private val generator = newGenerator()
    def hasNext = generator.hasNext
    def next() = generator.next()
  }
}
