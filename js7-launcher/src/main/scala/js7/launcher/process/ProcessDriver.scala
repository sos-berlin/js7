package js7.launcher.process

import cats.effect.IO
import cats.syntax.traverse.*
import js7.base.catsutils.CatsEffectExtensions.{left, startAndForget}
import js7.base.io.process.{KeyLogin, ProcessSignal, ReturnCode}
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, SetOnce}
import js7.data.job.{CommandLine, JobKey, ProcessExecutable}
import js7.data.order.{OrderId, OrderOutcome}
import js7.data.value.NamedValues
import js7.launcher.StdObservers
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.forwindows.{WindowsLogon, WindowsProcess}
import js7.launcher.process.ProcessDriver.*

final class ProcessDriver(
  orderId: OrderId,
  conf: Conf,
  jobLauncherConf: JobLauncherConf):

  import jobLauncherConf.crashPidFile
  private val checkedWindowsLogon = conf.login.traverse(WindowsLogon.fromKeyLogin)
  private lazy val returnValuesProvider = new ShellReturnValuesProvider(
    jobLauncherConf.tmpDirectory,
    jobLauncherConf.systemEncoding,
    v1Compatible = conf.v1Compatible)
  private val pipedProcessOnce = SetOnce[PipedProcess]
  private val startProcessLock = AsyncLock(orderId.toString)
  @volatile private var killedBeforeStart: Option[ProcessSignal] = None

  def runProcess(env: Map[String, Option[String]], stdObservers: StdObservers)
  : IO[OrderOutcome.Completed] =
    startProcess(env, stdObservers)
      .flatMap:
        case Left(problem) =>
          IO.pure(OrderOutcome.Failed.fromProblem(problem))

        case Right(process) =>
          crashPidFile
            .register(process.pid).surround:
              process.awaitProcessTermination
                .guarantee:
                  crashPidFile.remove(process.pid) // Immediately when process terminated
                .startAndForget
                .flatMap: _ =>
                  IO.defer:
                    logger.info:
                      s"$orderId ↘ Process ${process.process} started · ${conf.jobKey} · ${conf.commandLine}"
                    killedBeforeStart.traverse:
                      sendProcessSignal(process, _)
                .flatMap: _ =>
                  process.watchProcessAndStdouterr
                    .attempt.flatMap: either =>
                      IO.defer:
                        // Don't log PID because the process may have terminated long before
                        // stdout or stderr ended (due to still running child processes)
                        logger.info(s"$orderId ↙ Process completed with ${either.merge} after ${
                          process.duration.pretty}")
                        IO.fromEither(either)
                    .flatMap: returnCode =>
                      outcomeOf(returnCode)
          .guarantee:
            process.release
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
                  worryAboutStdoutAfterTermination = jobLauncherConf.worryAboutStdoutAfterTermination,
                  additionalEnvironment = env.updated(
                    returnValuesProvider.varName,
                    Some(returnValuesProvider.file.toString)),
                  maybeWindowsLogon)
          .flatMapT: processConfiguration =>
            startProcessLock.lock("startProcess"):
              GlobalStartProcessLock
                .lock(orderId.toString):
                  PipedProcess.start(conf.commandLine, processConfiguration, stdObservers,
                    orderId, conf.jobKey)
                .flatTapT: pipedProcess =>
                  IO:
                    pipedProcessOnce := pipedProcess
                    Checked.unit

  private def outcomeOf(returnCode: ReturnCode)
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
      pipedProcessOnce.toOption match
        case None =>
          IO:
            killedBeforeStart = Some(signal)
            logger.debug(s"$orderId ◼️ Kill before start")
        case Some(pipedProcess) =>
          sendProcessSignal(pipedProcess, signal))

  private def sendProcessSignal(pipedProcess: PipedProcess, signal: ProcessSignal): IO[Unit] =
    pipedProcess.sendProcessSignal(signal)

  override def toString = s"ProcessDriver(${conf.jobKey})"


object ProcessDriver:
  private val logger = Logger[this.type]

  /** Linux may return a "busy" error when starting many processes at once. */
  private val GlobalStartProcessLock = AsyncLock("Process start")

  private[process] final case class Conf(
    jobKey: JobKey,
    toOutcome: (NamedValues, ReturnCode) => OrderOutcome.Completed,
    commandLine: CommandLine,
    login: Option[KeyLogin] = None,
    v1Compatible: Boolean = false)
