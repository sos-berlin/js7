package js7.launcher.process

import cats.effect.IO
import cats.syntax.traverse.*
import js7.base.catsutils.CatsEffectExtensions.left
import js7.base.io.process.{KeyLogin, ProcessSignal, ReturnCode}
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, SetOnce}
import js7.data.job.TaskId.newGenerator
import js7.data.job.{CommandLine, JobKey, ProcessExecutable, TaskId}
import js7.data.order.{OrderId, OrderOutcome}
import js7.data.value.NamedValues
import js7.launcher.StdObservers
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.forwindows.{WindowsLogon, WindowsProcess}
import js7.launcher.process.PipedProcess.start
import js7.launcher.process.ProcessDriver.*
import scala.collection.AbstractIterator

final class ProcessDriver(
  orderId: OrderId,
  conf: Conf,
  jobLauncherConf: JobLauncherConf):

  import jobLauncherConf.implicitIox

  private val taskId = taskIdGenerator.next()
  private val checkedWindowsLogon = conf.login.traverse(WindowsLogon.fromKeyLogin)
  private lazy val returnValuesProvider = new ShellReturnValuesProvider(
    jobLauncherConf.tmpDirectory,
    jobLauncherConf.systemEncoding,
    v1Compatible = conf.v1Compatible)
  //private val terminatedPromise = Deferred.unsafe[IO, Either[Throwable, Completed]]
  private val richProcessOnce = SetOnce[PipedProcess]
  private val startProcessLock = AsyncLock(orderId.toString)
  @volatile private var killedBeforeStart: Option[ProcessSignal] = None

  def runProcess(env: Map[String, Option[String]], stdObservers: StdObservers)
  : IO[OrderOutcome.Completed] =
    startProcess(env, stdObservers)
      .flatMap:
        case Left(problem) =>
          IO.pure(OrderOutcome.Failed.fromProblem(problem))

        case Right(process) =>
          IO.defer:
            logger.info:
              s"$orderId ↘ Process $process started, ${conf.jobKey}: ${conf.commandLine}"
            killedBeforeStart.traverse:
              sendProcessSignal(process, _)
            .flatMap: _ =>
              process.watchProcessAndStdouterr
                .attempt.flatMap: either =>
                  IO.defer:
                    // Don't log PID because the process may have terminated long before
                    // stdout or stderr ended (due to still running child processes)
                    logger.info:
                      s"$orderId ↙ Process completed with ${either.merge} after ${process.duration.pretty}"
                    IO.fromEither(either)
                .flatMap: returnCode =>
                  outcomeOf(process, returnCode)
      //.guarantee:
      //  stdObservers.closeChannels // Close stdout and stderr streams (only for internal jobs)

  private def startProcess(env: Map[String, Option[String]], stdObservers: StdObservers)
  : IO[Checked[PipedProcess]] =
    IO.defer:
      killedBeforeStart match
        case Some(signal) =>
          IO.left(Problem.pure("Processing killed before start"))

        case None =>
          IO:
            checkedWindowsLogon.flatMap: maybeWindowsLogon =>
              catchNonFatal:
                for o <- maybeWindowsLogon do
                  WindowsProcess.makeFileAppendableForUser(returnValuesProvider.file, o.userName)
              .map: _ =>
                ProcessConfiguration(
                  workingDirectory = Some(jobLauncherConf.workingDirectory),
                  encoding = jobLauncherConf.systemEncoding,
                  additionalEnvironment = env.updated(
                    returnValuesProvider.varName,
                    Some(returnValuesProvider.file.toString)),
                  maybeTaskId = Some(taskId),
                  maybeKillScript = jobLauncherConf.killScript,
                  maybeWindowsLogon)
          .flatMapT: processConfiguration =>
            startProcessLock.lock("startProcess"):
              GlobalStartProcessLock
                .lock(orderId.toString):
                  PipedProcess.start(conf.commandLine, processConfiguration, stdObservers,
                    orderId, conf.jobKey)
                .flatTapT: richProcess =>
                  IO:
                    richProcessOnce := richProcess
                    Checked.unit

  private def outcomeOf(richProcess: PipedProcess, returnCode: ReturnCode)
  : IO[OrderOutcome.Completed] =
    fetchReturnValuesThenDeleteFile.map:
      case Left(problem) =>
        OrderOutcome.Failed.fromProblem(
          problem.withPrefix("Reading return values failed:"),
          Map(ProcessExecutable.toNamedValue(returnCode)))

      case Right(namedValues) =>
        conf.toOutcome(namedValues, returnCode)
    .guarantee(IO.interruptible:
      returnValuesProvider.tryDeleteFile())

  private def fetchReturnValuesThenDeleteFile: IO[Checked[NamedValues]] =
    IO.interruptible:
      catchNonFatal:
        val result = returnValuesProvider.read()
        returnValuesProvider.tryDeleteFile()
        result
    .logWhenItTakesLonger(s"fetchReturnValuesThenDeleteFile $orderId") // Because IO.interruptible does not execute ?

  def kill(signal: ProcessSignal): IO[Unit] =
    startProcessLock.lock("kill")(IO.defer:
      richProcessOnce.toOption match
        case None =>
          IO:
            killedBeforeStart = Some(signal)
            logger.debug(s"$orderId ⚫️ Kill before start")
        case Some(richProcess) =>
          sendProcessSignal(richProcess, signal))

  private def sendProcessSignal(richProcess: PipedProcess, signal: ProcessSignal): IO[Unit] =
    richProcess.sendProcessSignal(signal)

  override def toString = s"ProcessDriver($taskId ${conf.jobKey})"


object ProcessDriver:
  private val logger = Logger[this.type]

  /** Linux may return a "busy" error when starting many processes at once. */
  private val GlobalStartProcessLock = AsyncLock("Process start")

  private object taskIdGenerator extends AbstractIterator[TaskId]:
    private val generator = newGenerator()
    def hasNext = generator.hasNext
    def next() = generator.next()

  private[process] final case class Conf(
    jobKey: JobKey,
    toOutcome: (NamedValues, ReturnCode) => OrderOutcome.Completed,
    commandLine: CommandLine,
    login: Option[KeyLogin] = None,
    v1Compatible: Boolean = false)
