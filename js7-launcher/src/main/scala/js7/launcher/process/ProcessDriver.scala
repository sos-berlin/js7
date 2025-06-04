package js7.launcher.process

import cats.effect.IO
import cats.syntax.traverse.*
import js7.base.catsutils.CatsEffectExtensions.{catchAsChecked, left, startAndForget}
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
  private val pipedProcessOnce = SetOnce[PipedProcess]
  private val startProcessLock = AsyncLock(orderId.toString)
  @volatile private var killed = false
  @volatile private var killedBeforeStart: Option[ProcessSignal] = None

  def runProcess(env: Map[String, Option[String]], stdObservers: StdObservers)
  : IO[OrderOutcome.Completed] =
    ShellReturnValuesProvider.resource(
        jobLauncherConf.tmpDirectory,
        jobLauncherConf.systemEncoding,
        v1Compatible = conf.v1Compatible)
      .use: returnValuesProvider =>
        new Run(env, stdObservers, returnValuesProvider).runProcess

  private class Run(
    env: Map[String, Option[String]],
    stdObservers: StdObservers,
    returnValuesProvider: ShellReturnValuesProvider):

    def runProcess: IO[OrderOutcome.Completed] =
      startProcess.flatMap:
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
                      fetchReturnValues(returnCode)
          .guarantee:
            process.release
      //.guarantee:
      //  stdObservers.closeChannels // Close stdout and stderr streams (only for internal jobs)

    private def startProcess: IO[Checked[PipedProcess]] =
      IO.defer:
        if killedBeforeStart.isDefined then
          IO.left(Problem.pure("Processing killed before start"))
        else
          IO:
            conf.login.traverse(WindowsLogon.fromKeyLogin).flatMap: maybeWindowsLogon =>
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

    private def fetchReturnValues(returnCode: ReturnCode): IO[OrderOutcome.Completed] =
      if killed then
        IO.pure(conf.toOutcome(NamedValues.empty, returnCode))
      else
        returnValuesProvider.read.catchAsChecked
          .logWhenItTakesLonger(s"fetchReturnValues $orderId") // Because IO.interruptible does not execute ?
          .map:
            case Left(problem) =>
              OrderOutcome.Failed.fromProblem(
                problem.withPrefix("Reading return values file failed:"),
                Map(ProcessExecutable.toNamedValue(returnCode)))

            case Right(namedValues) =>
              conf.toOutcome(namedValues, returnCode)

  def kill(signal: ProcessSignal): IO[Unit] =
    startProcessLock.lock("kill"):
      IO.defer:
        pipedProcessOnce.toOption match
          case None =>
            IO:
              killedBeforeStart = Some(signal)
              logger.debug(s"$orderId ◼️ Kill before start")
          case Some(pipedProcess) =>
            sendProcessSignal(pipedProcess, signal)

  private def sendProcessSignal(pipedProcess: PipedProcess, signal: ProcessSignal): IO[Unit] =
    IO.defer:
      killed = true
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
