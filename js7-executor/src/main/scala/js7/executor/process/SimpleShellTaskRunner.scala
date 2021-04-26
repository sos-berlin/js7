package js7.executor.process

import java.nio.file.Path
import javax.inject.Singleton
import js7.base.generic.Completed
import js7.base.io.process.{ProcessSignal, ReturnCode}
import js7.base.log.Logger
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{SetOnce, TaskLock}
import js7.data.job.TaskId
import js7.data.job.TaskId.newGenerator
import js7.data.order.{OrderId, Outcome}
import js7.data.value.NamedValues
import js7.executor.StdObservers
import js7.executor.configuration.{JobExecutorConf, ProcessKillScript, TaskConfiguration}
import js7.executor.process.ShellScriptProcess.startPipedShellScript
import js7.executor.process.SimpleShellTaskRunner._
import js7.executor.task.{BaseAgentTask, TaskRunner}
import monix.eval.Task
import scala.concurrent.Promise
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class SimpleShellTaskRunner(
  conf: TaskConfiguration,
  taskId: TaskId,
  temporaryDirectory: Path,
  workingDirectory: Path,
  killScript: Option[ProcessKillScript])
  (implicit iox: IOExecutor)
extends TaskRunner
{
  private val startProcessLock = TaskLock("syncStartProcess")

  val asBaseAgentTask = new BaseAgentTask {
    def id = taskId
    def jobKey = conf.jobKey
    def pidOption = richProcessOnce.toOption.flatMap(_.pidOption)
    def terminated = terminatedPromise.future

    def sendProcessSignal(signal: ProcessSignal) =
      for (o <- richProcessOnce) o.sendProcessSignal(signal)

    override def toString = SimpleShellTaskRunner.this.toString
  }

  private val terminatedPromise = Promise[Completed]()
  private lazy val returnValuesProvider = new ShellReturnValuesProvider(
    temporaryDirectory,
    v1Compatible = conf.v1Compatible)
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
    for (returnCode <- runProcess(orderId, env, stdObservers)) yield
      conf.toOutcome(fetchReturnValuesThenDeleteFile(), returnCode)

  private def runProcess(orderId: OrderId, env: Map[String, String], stdObservers: StdObservers): Task[ReturnCode] =
    for {
      richProcess <- startProcess(env, stdObservers)
      _ <- Task {
        logger.info(s"Process $richProcess started for $orderId, ${conf.jobKey}: ${conf.commandLine}")
      }
      tried <- richProcess.terminated.materialize
      _ <- Task {
        logger.info(s"Process '$richProcess' terminated with ${tried getOrElse tried} after ${richProcess.duration.pretty}")
      }
      returnCode <- Task.fromTry(tried)
    } yield {
      richProcess.close()
      returnCode
    }

  private def fetchReturnValuesThenDeleteFile(): NamedValues = {
    val result = returnValuesProvider.read() // TODO Catch exceptions
    // TODO When Windows locks the file, try delete it later, asynchronously, and block file in FilePool
    try returnValuesProvider.deleteFile()
    catch { case NonFatal(t) =>
      logger.error(s"Cannot delete file '$returnValuesProvider': ${t.toStringWithCauses}")
      throw t
    }
    result
  }

  private def startProcess(env: Map[String, String], stdObservers: StdObservers): Task[RichProcess] =
    Task.deferAction { implicit scheduler =>
      if (killedBeforeStart)
        Task.raiseError(new RuntimeException(s"$taskId killed before start"))
      else {
        val processConfiguration = ProcessConfiguration(
          stdFileMap = Map.empty,
          encoding = JobExecutorConf.FileEncoding,
          workingDirectory = Some(workingDirectory),
          additionalEnvironment = env + returnValuesProvider.toEnv,
          maybeTaskId = Some(taskId),
          killScriptOption = killScript)
        startProcessLock
          .lock(Task {
            startPipedShellScript(conf.commandLine, processConfiguration, stdObservers)
          })
          .map { richProcess =>
            terminatedPromise.completeWith(richProcess.terminated.map(_ => Completed).runToFuture)
            richProcessOnce := richProcess
          }
      }
    }

  def kill(signal: ProcessSignal): Unit =
    richProcessOnce.toOption match {
      case Some(richProcess) =>
        richProcess.sendProcessSignal(signal)
      case None =>
        terminatedPromise.tryFailure(new RuntimeException(s"$taskId killed before start"))
        killedBeforeStart = true
    }

  override def toString = s"SimpleShellTaskRunner($taskId ${conf.jobKey})"
}

object SimpleShellTaskRunner
{
  private val logger = Logger(getClass)

  @Singleton
  final class TaskIdGenerator extends Iterator[TaskId] {
    private val generator = newGenerator()
    def hasNext = generator.hasNext
    def next() = generator.next()
  }
}
