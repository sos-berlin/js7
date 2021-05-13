package js7.executor.process

import cats.syntax.traverse._
import js7.base.generic.Completed
import js7.base.io.process.{ProcessSignal, ReturnCode}
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{SetOnce, TaskLock}
import js7.data.job.TaskId
import js7.data.job.TaskId.newGenerator
import js7.data.order.{OrderId, Outcome}
import js7.data.value.NamedValues
import js7.executor.StdObservers
import js7.executor.configuration.{JobExecutorConf, TaskConfiguration}
import js7.executor.forwindows.{WindowsLogon, WindowsProcess}
import js7.executor.process.ProcessDriver._
import js7.executor.process.ShellScriptProcess.startPipedShellScript
import monix.eval.Task
import scala.concurrent.Promise
import scala.util.control.NonFatal

final class ProcessDriver(
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
  private var killedBeforeStart = false

  def terminate: Task[Unit] =
    Task.defer {
      returnValuesProvider.deleteFile()
      richProcessOnce.toOption match {
        case Some(richProcess) =>
          richProcess.terminated.map((_: ReturnCode) => ())
        case None =>
          Task.unit
      }
    }

  def processOrder(orderId: OrderId, env: Map[String, String], stdObservers: StdObservers)
  : Task[Outcome.Completed] =
    runProcess(orderId, env, stdObservers)
      .map {
        case Left(problem) => Outcome.Failed.fromProblem(problem)
        case Right(returnCode) => conf.toOutcome(fetchReturnValuesThenDeleteFile(), returnCode)
      }

  private def runProcess(orderId: OrderId, env: Map[String, String], stdObservers: StdObservers)
  : Task[Checked[ReturnCode]] =
    startProcess(env, stdObservers)
      .flatMapT { richProcess =>
        logger.info(s"Process $richProcess started for $orderId, ${conf.jobKey}: ${conf.commandLine}")
        richProcess.terminated.materialize
          .flatMap { tried =>
            logger.info(s"Process $richProcess terminated with ${tried getOrElse tried} after ${richProcess.duration.pretty}")
            Task.fromTry(tried.map(Right(_)))
          }
      }

  private def fetchReturnValuesThenDeleteFile(): NamedValues = {
    val result = returnValuesProvider.read() // TODO Catch exceptions
    // TODO When Windows locks the file, try delete it later, asynchronously
    try returnValuesProvider.deleteFile()
    catch { case NonFatal(t) =>
      logger.error(s"Cannot delete file '$returnValuesProvider': ${t.toStringWithCauses}")
      throw t
    }
    result
  }

  private def startProcess(env: Map[String, String], stdObservers: StdObservers)
  : Task[Checked[RichProcess]] =
    Task.deferAction { implicit scheduler =>
      if (killedBeforeStart)
        Task.raiseError(new RuntimeException(s"$taskId killed before start"))
      else
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
        ) .flatMapT(processConfiguration =>
            startProcessLock/*required due to a Linux problem with many process starts ???*/
              .lock(
                startPipedShellScript(conf.commandLine, processConfiguration, stdObservers))
              .map(_.map { richProcess =>
                terminatedPromise.completeWith(richProcess.terminated.map(_ => Completed).runToFuture)
                richProcessOnce := richProcess
              }))
    }

  def kill(signal: ProcessSignal): Unit =
    richProcessOnce.toOption match {
      case Some(richProcess) =>
        richProcess.sendProcessSignal(signal)
      case None =>
        terminatedPromise.tryFailure(new RuntimeException(s"$taskId killed before start"))
        killedBeforeStart = true
    }

  override def toString = s"ProcessDriver($taskId ${conf.jobKey})"
}

object ProcessDriver
{
  private val logger = Logger(getClass)

  private val startProcessLock = TaskLock("syncStartProcess")

  private object taskIdGenerator extends Iterator[TaskId] {
    private val generator = newGenerator()
    def hasNext = generator.hasNext
    def next() = generator.next()
  }
}
