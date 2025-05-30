package js7.launcher.process

import cats.effect.{Deferred, FiberIO, IO, Outcome}
import cats.syntax.option.*
import java.io.{IOException, InputStream}
import java.lang.ProcessBuilder.Redirect.PIPE
import js7.base.catsutils.CatsEffectExtensions.{joinStd, raceBoth, startAndForget}
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.io.process.ProcessExtensions.onExitIO
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.io.process.Processes.*
import js7.base.io.process.StartRobustly.startRobustly
import js7.base.io.process.{JavaProcess, Js7Process, Pid, ProcessSignal, ReturnCode, Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked
import js7.base.system.OperatingSystem.isWindows
import js7.base.thread.IOExecutor.env.interruptibleVirtualThread
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Worry.AfterTenSecondsWorryDurations
import js7.base.utils.{Allocated, Atomic, BlockingLock, Worry}
import js7.data.job.{CommandLine, JobKey}
import js7.data.order.OrderId
import js7.launcher.StdObservers
import js7.launcher.forwindows.WindowsProcess
import js7.launcher.forwindows.WindowsProcess.StartWindowsProcess
import js7.launcher.process.PipedProcess.*
import js7.launcher.processkiller.SubagentProcessKiller
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.jdk.CollectionConverters.*

final class PipedProcess private(
  val conf: ProcessConfiguration,
  private[process] val process: Js7Process,
  stdObservers: StdObservers,
  orderId: OrderId,
  jobKey: JobKey,
  processKillerAlloc: Allocated[IO, SubagentProcessKiller],
  label: String):

  private val logger = Logger.withPrefix[this.type](label)
  private val processKiller = processKillerAlloc.allocatedThing
  private val sigkilled = Deferred.unsafe[IO, Unit]
  private var _isKilling = none[ProcessSignal]
  private val _isKillingLock = BlockingLock()

  private val runningSince = now

  def pid: Pid =
    process.pid

  def duration: FiniteDuration = runningSince.elapsed

  val awaitProcessTermination: IO[ReturnCode] =
    memoize:
      process.maybeHandle.fold(IO.unit)(_.onExitIO) *>
        IO.defer:
          process.returnCode.map(IO.pure)
            .getOrElse:
              interruptibleVirtualThread:
                logger.traceCallWithResult(s"waitFor $process"):
                  process.waitFor()
      .flatTap: rc =>
        IO(logger.trace(s"Process $pid terminated with $rc"))

  val watchProcessAndStdouterr: IO[ReturnCode] =
    memoize:
      IO.raceBoth(
          awaitProcessTermination,
          IO.raceBoth(
              sigkilled.get.andWait(killStdoutAndStderrDelay),
              pumpStdoutAndStderrToSink)
            .flatMap:
              case Left((), stdouterrFiber) =>
                IO.defer:
                  logger.warn:
                    s"Ignoring stdout and stderr after SIGKILL (maybe a child process is still running)"
                  joinStdouterr(stdouterrFiber, orderId, jobKey, stdoutAndStderrAbandonAfter)
                    .startAndForget
                .as(false /*stdout ignored*/)
              case Right((sigkilledFiber, ())) =>
                sigkilledFiber.cancel
                  .as(true /*stdout ended*/))
        .flatMap:
          case Left((returnCode, stdouterrFiber)) => // Process terminated
            IO.defer:
              logger.debug(s"terminated with $returnCode")
              IO:
                logger.info:
                  s"terminated with $returnCode, awaiting stdout or stderr (maybe a child process is still running)"
              .delayBy(conf.worryAboutStdoutAfterTermination)
              .background.surround:
                val what = s"$orderId stdout or stderr"
                stdouterrFiber.joinStd
                  .logWhenItTakesLonger(StdouterrWorry):
                    case (None, elapsed, _, sym) => IO.pure:
                      s"$sym Still waiting for $what for ${elapsed.pretty}"
                    case (Some(Outcome.Succeeded(ended)), elapsed, _, _) =>
                      ended.flatMap(ended => IO:
                        if ended
                        then s"🔵 $what ended after ${elapsed.pretty}"
                        else s"🟣 $what are still ignored after ${elapsed.pretty}")
                    case (Some(Outcome.Canceled()), elapsed, _, sym) => IO.pure:
                      s"$sym $what canceled after ${elapsed.pretty}"
                    case (Some(Outcome.Errored(t)), elapsed, _, sym) => IO.pure:
                      s"$sym $what failed after ${elapsed.pretty} with ${t.toStringWithCauses}"
            .as(returnCode)

          case Right((terminationFiber, _)) =>
            // Stdout and stderr ended or ignored after SIGKILL
            terminationFiber.joinStd

  def release: IO[Unit] =
    processKillerAlloc.release

  private def pumpStdoutAndStderrToSink: IO[Unit] =
    logger.traceIO(s"pumpStdoutAndStderrToSink")(IO
      .both(
        pumpOutErrToSink(Stdout, process.stdout),
        pumpOutErrToSink(Stderr, process.stderr))
      .void)

  private def pumpOutErrToSink(outErr: StdoutOrStderr, in: InputStream): IO[Unit] =
    stdObservers
      .pumpInputStreamToSink(outErr, in, conf.encoding)
      .handleErrorWith:
        case t: IOException if isWindows && _isKilling.isDefined => IO:
          logger.warn(
            s"While killing the process, $outErr became unreadable: ${t.toStringWithCauses}",
            t)
        case t => IO(logger.warn(s"$outErr: ${t.toStringWithCauses}"))

  def sendProcessSignal(signal: ProcessSignal): IO[Unit] =
    IO.defer:
      val force = signal == SIGKILL
      _isKillingLock.lock:
        if !_isKilling.contains(SIGKILL) then
          _isKilling = Some(signal)
      kill(force)
        .guarantee:
          IO.whenA(force):
            sigkilled.complete(()).void

  private def kill(force: Boolean): IO[Unit] =
    if !force then
      processKiller.sigtermMainProcessAndSaveDescendant(process)
    else
      // Kill saved descendants, too
      processKiller.sigkillWithDescendants(process)

  @TestOnly
  private[process] def isAlive = process.isAlive

  override def toString = label


object PipedProcess:

  private val logger = Logger[this.type]
  /** Grace period between SIGKILL and (second) destroyForcibly. */
  private val killStdoutAndStderrDelay = 500.ms
  private val stdoutAndStderrAbandonAfter = 3.s
  private val StdouterrWorry = Worry(
    List(1.s, 3.s, 6.s) ::: AfterTenSecondsWorryDurations,
    infoLevel = 0.s, orangeLevel = 3.s)

  private val pumpFiberCount = Atomic(0)

  /** Start a process and read stdout and stderr as pipes. */
  def start(
    commandLine: CommandLine,
    conf: ProcessConfiguration,
    stdObservers: StdObservers,
    orderId: OrderId,
    jobKey: JobKey)
  : IO[Checked[PipedProcess]] =
    // TODO Make ResourceIO[PipedProcess]
    IO.defer:
      val commandArgs = toShellCommandArguments(commandLine.file, commandLine.arguments.tail)
      // Check argsToCommandLine here to avoid exception in WindowsProcess.start
      startProcess(commandArgs, conf)
        .flatMapT: process =>
          process.stdin.close() // Process gets an empty stdin
          val label = s"$orderId $process"
          SubagentProcessKiller.resource(label).toAllocated
            .map: killer =>
              Right:
                PipedProcess(conf, process, stdObservers, orderId, jobKey, killer, label)

  private def startProcess(args: Seq[String], conf: ProcessConfiguration)
  : IO[Checked[Js7Process]] =
    conf.windowsLogon match
      case None =>
        val processBuilder = new ProcessBuilder(args.asJava)
        for o <- conf.workingDirectory do processBuilder.directory(o.toFile)

        transferEnv(from = conf.additionalEnvironment, to = processBuilder.environment)

        processBuilder.startRobustly()
          .map(o => Right(JavaProcess(o)))

      case Some(logon) =>
        IO.blocking:
          WindowsProcess.startWithWindowsLogon(
            StartWindowsProcess(
              args,
              stdinRedirect = PIPE,
              stdoutRedirect = PIPE,
              stderrRedirect = PIPE,
              additionalEnv = conf.additionalEnvironment),
            Some(logon))

  private def transferEnv(
    from: Map[String, Option[String]],
    to: java.util.Map[String, String])
  : Unit =
    from
      .collect { case (k, None) => k }
      .foreach(to.remove)

    to.putAll(from
      .collect { case (k, Some(v)) => k -> v }
      .asJava)

  private def joinStdouterr(
    fiber: FiberIO[Unit],
    orderId: OrderId,
    jobKey: JobKey,
    timeout: FiniteDuration)
  : IO[Unit] =
    IO.defer:
      val since = Deadline.now
      var logged = false
      import pumpFiberCount as n
      n += 1
      IO
        .raceBoth(
          fiber.joinStd
            .*>(IO:
              if logged then
                // Not reliable due to race condition
                logger.info:
                  s"🟣 $orderId $jobKey: ignored stdout and stderr have finally ended after ${
                    since.elapsed.pretty}${(n.get > 1) ?? s" (n=$n)"}")
            .guarantee:
              IO(n -= 1).void,
          IO.defer:
            IO.sleep(timeout) *> IO:
              // Order has proceeded by now
              logged = true
              logger.info:
                s"🟣 $orderId $jobKey: still ignoring stdout and stderr after process termination ${
                  since.elapsed.pretty} ago${(n.get > 1) ?? s" (n=$n)"}")
        .flatMap:
          case Left(((), logging)) => logging.cancel
          case Right((_, ())) => IO.unit // Forget fiber
